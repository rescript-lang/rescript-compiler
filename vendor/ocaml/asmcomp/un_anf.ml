(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* We say that an [Ident.t] is "linear" iff:
   (a) it is used exactly once;
   (b) it is never assigned to (using [Uassign]).
*)
type ident_info =
  { used : Ident.Set.t;
    linear : Ident.Set.t;
    assigned : Ident.Set.t;
    closure_environment : Ident.Set.t;
    let_bound_vars_that_can_be_moved : Ident.Set.t;
  }

let ignore_uconstant (_ : Clambda.uconstant) = ()
let ignore_ulambda (_ : Clambda.ulambda) = ()
let ignore_ulambda_list (_ : Clambda.ulambda list) = ()
let ignore_function_label (_ : Clambda.function_label) = ()
let ignore_debuginfo (_ : Debuginfo.t) = ()
let ignore_int (_ : int) = ()
let ignore_ident (_ : Ident.t) = ()
let ignore_ident_option (_ : Ident.t option) = ()
let ignore_primitive (_ : Lambda.primitive) = ()
let ignore_string (_ : string) = ()
let ignore_int_array (_ : int array) = ()
let ignore_ident_list (_ : Ident.t list) = ()
let ignore_direction_flag (_ : Asttypes.direction_flag) = ()
let ignore_meth_kind (_ : Lambda.meth_kind) = ()

(* CR-soon mshinwell: check we aren't traversing function bodies more than
   once (need to analyse exactly what the calls are from Cmmgen into this
   module). *)

let closure_environment_ident (ufunction:Clambda.ufunction) =
  (* The argument after the arity is the environment *)
  if List.length ufunction.params = ufunction.arity + 1 then
    let env_var = List.nth ufunction.params ufunction.arity in
    assert(Ident.name env_var = "env");
    Some env_var
  else
    (* closed function, no environment *)
    None

let make_ident_info (clam : Clambda.ulambda) : ident_info =
  let t : int Ident.Tbl.t = Ident.Tbl.create 42 in
  let assigned_idents = ref Ident.Set.empty in
  let environment_idents = ref Ident.Set.empty in
  let rec loop : Clambda.ulambda -> unit = function
    (* No underscores in the pattern match, to reduce the chance of failing
       to traverse some subexpression. *)
    | Uvar id ->
      begin match Ident.Tbl.find t id with
      | n -> Ident.Tbl.replace t id (n + 1)
      | exception Not_found -> Ident.Tbl.add t id 1
      end
    | Uconst const ->
      (* The only variables that might occur in [const] are those in constant
         closures---and those are all bound by such closures.  It follows that
         [const] cannot contain any variables that are bound in the current
         scope, so we do not need to count them here.  (The function bodies
         of the closures will be traversed when this function is called from
         [Cmmgen.transl_function].) *)
      ignore_uconstant const
    | Udirect_apply (label, args, dbg) ->
      ignore_function_label label;
      List.iter loop args;
      ignore_debuginfo dbg
    | Ugeneric_apply (func, args, dbg) ->
      loop func;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uclosure (functions, captured_variables) ->
      List.iter loop captured_variables;
      List.iter (fun (
        { Clambda. label; arity; params; body; dbg; env; } as clos) ->
          (match closure_environment_ident clos with
           | None -> ()
           | Some env_var ->
             environment_idents :=
               Ident.Set.add env_var !environment_idents);
          ignore_function_label label;
          ignore_int arity;
          ignore_ident_list params;
          loop body;
          ignore_debuginfo dbg;
          ignore_ident_option env)
        functions
    | Uoffset (expr, offset) ->
      loop expr;
      ignore_int offset
    | Ulet (_let_kind, _value_kind, _ident, def, body) ->
      loop def;
      loop body
    | Uletrec (defs, body) ->
      List.iter (fun (ident, def) ->
          ignore_ident ident;
          loop def)
        defs;
      loop body
    | Uprim (prim, args, dbg) ->
      ignore_primitive prim;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uswitch (cond, { us_index_consts; us_actions_consts;
          us_index_blocks; us_actions_blocks }, dbg) ->
      loop cond;
      ignore_int_array us_index_consts;
      Array.iter loop us_actions_consts;
      ignore_int_array us_index_blocks;
      Array.iter loop us_actions_blocks;
      ignore_debuginfo dbg
    | Ustringswitch (cond, branches, default) ->
      loop cond;
      List.iter (fun (str, branch) ->
          ignore_string str;
          loop branch)
        branches;
      Misc.may loop default
    | Ustaticfail (static_exn, args) ->
      ignore_int static_exn;
      List.iter loop args
    | Ucatch (static_exn, idents, body, handler) ->
      ignore_int static_exn;
      ignore_ident_list idents;
      loop body;
      loop handler
    | Utrywith (body, ident, handler) ->
      loop body;
      ignore_ident ident;
      loop handler
    | Uifthenelse (cond, ifso, ifnot) ->
      loop cond;
      loop ifso;
      loop ifnot
    | Usequence (e1, e2) ->
      loop e1;
      loop e2
    | Uwhile (cond, body) ->
      loop cond;
      loop body
    | Ufor (ident, low, high, direction_flag, body) ->
      ignore_ident ident;
      loop low;
      loop high;
      ignore_direction_flag direction_flag;
      loop body
    | Uassign (ident, expr) ->
      assigned_idents := Ident.Set.add ident !assigned_idents;
      loop expr
    | Usend (meth_kind, e1, e2, args, dbg) ->
      ignore_meth_kind meth_kind;
      loop e1;
      loop e2;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uunreachable ->
      ()
  in
  loop clam;
  let linear =
    Ident.Tbl.fold (fun id n acc ->
        assert (n >= 1);
        if n = 1 && not (Ident.Set.mem id !assigned_idents)
        then Ident.Set.add id acc
        else acc)
      t Ident.Set.empty
  in
  let assigned = !assigned_idents in
  let used =
    (* This doesn't work transitively and thus is somewhat restricted.  In
       particular, it does not allow us to get rid of useless chains of [let]s.
       However it should be sufficient to remove the majority of unnecessary
       [let] bindings that might hinder [Cmmgen]. *)
    Ident.Tbl.fold (fun id _n acc -> Ident.Set.add id acc)
      t assigned
  in
  { used; linear; assigned; closure_environment = !environment_idents;
    let_bound_vars_that_can_be_moved = Ident.Set.empty;
  }

(* When sequences of [let]-bindings match the evaluation order in a subsequent
   primitive or function application whose arguments are linearly-used
   non-assigned variables bound by such lets (possibly interspersed with other
   variables that are known to be constant), and it is known that there were no
   intervening side-effects during the evaluation of the [let]-bindings,
   permit substitution of the variables for their defining expressions. *)
let let_bound_vars_that_can_be_moved ident_info (clam : Clambda.ulambda) =
  let obviously_constant = ref Ident.Set.empty in
  let can_move = ref Ident.Set.empty in
  let let_stack = ref [] in
  let examine_argument_list args =
    let rec loop let_bound_vars (args : Clambda.ulambda list) =
      match let_bound_vars, args with
      | _, [] ->
        (* We've matched all arguments and will not substitute (in the
           current application being considered) any of the remaining
           [let_bound_vars].  As such they may stay on the stack. *)
        let_bound_vars
      | [], _ ->
        (* There are no more [let]-bindings to consider, so the stack
           is left empty. *)
        []
      | let_bound_vars, (Uvar arg)::args
          when Ident.Set.mem arg !obviously_constant ->
        loop let_bound_vars args
      | let_bound_var::let_bound_vars, (Uvar arg)::args
          when Ident.same let_bound_var arg
            && not (Ident.Set.mem arg ident_info.assigned) ->
        assert (Ident.Set.mem arg ident_info.used);
        assert (Ident.Set.mem arg ident_info.linear);
        can_move := Ident.Set.add arg !can_move;
        loop let_bound_vars args
      | _::_, _::_ ->
        (* The [let] sequence has ceased to match the evaluation order
           or we have encountered some complicated argument.  In this case
           we empty the stack to ensure that we do not end up moving an
           outer [let] across a side effect. *)
        []
    in
    (* Start at the most recent let binding and the leftmost argument
       (the last argument to be evaluated). *)
    let_stack := loop !let_stack args
  in
  let rec loop : Clambda.ulambda -> unit = function
    | Uvar ident ->
      if Ident.Set.mem ident ident_info.assigned then begin
        let_stack := []
      end
    | Uconst const ->
      ignore_uconstant const
    | Udirect_apply (label, args, dbg) ->
      ignore_function_label label;
      examine_argument_list args;
      (* We don't currently traverse [args]; they should all be variables
         anyway.  If this is added in the future, take care to traverse [args]
         following the evaluation order. *)
      ignore_debuginfo dbg
    | Ugeneric_apply (func, args, dbg) ->
      examine_argument_list (args @ [func]);
      ignore_debuginfo dbg
    | Uclosure (functions, captured_variables) ->
      ignore_ulambda_list captured_variables;
      (* Start a new let stack for speed. *)
      List.iter (fun { Clambda. label; arity; params; body; dbg; env; } ->
          ignore_function_label label;
          ignore_int arity;
          ignore_ident_list params;
          let_stack := [];
          loop body;
          let_stack := [];
          ignore_debuginfo dbg;
          ignore_ident_option env)
        functions
    | Uoffset (expr, offset) ->
      (* [expr] should usually be a variable. *)
      examine_argument_list [expr];
      ignore_int offset
    | Ulet (_let_kind, _value_kind, ident, def, body) ->
      begin match def with
      | Uconst _ ->
        (* The defining expression is obviously constant, so we don't
           have to put this [let] on the stack, and we don't have to
           traverse the defining expression either. *)
        obviously_constant := Ident.Set.add ident !obviously_constant;
        loop body
      | _ ->
        loop def;
        if Ident.Set.mem ident ident_info.linear then begin
          let_stack := ident::!let_stack
        end else begin
          (* If we encounter a non-linear [let]-binding then we must clear
             the let stack, since we cannot now move any previous binding
             across the non-linear one. *)
          let_stack := []
        end;
        loop body
      end
    | Uletrec (defs, body) ->
      (* Evaluation order for [defs] is not defined, and this case
         probably isn't important for [Cmmgen] anyway. *)
      let_stack := [];
      List.iter (fun (ident, def) ->
          ignore_ident ident;
          loop def;
          let_stack := [])
        defs;
      loop body
    | Uprim (prim, args, dbg) ->
      ignore_primitive prim;
      examine_argument_list args;
      ignore_debuginfo dbg
    | Uswitch (cond, { us_index_consts; us_actions_consts;
          us_index_blocks; us_actions_blocks }, dbg) ->
      examine_argument_list [cond];
      ignore_int_array us_index_consts;
      Array.iter (fun action ->
          let_stack := [];
          loop action)
        us_actions_consts;
      ignore_int_array us_index_blocks;
      Array.iter (fun action ->
          let_stack := [];
          loop action)
        us_actions_blocks;
      ignore_debuginfo dbg;
      let_stack := []
    | Ustringswitch (cond, branches, default) ->
      examine_argument_list [cond];
      List.iter (fun (str, branch) ->
          ignore_string str;
          let_stack := [];
          loop branch)
        branches;
      let_stack := [];
      Misc.may loop default;
      let_stack := []
    | Ustaticfail (static_exn, args) ->
      ignore_int static_exn;
      examine_argument_list args
    | Ucatch (static_exn, idents, body, handler) ->
      ignore_int static_exn;
      ignore_ident_list idents;
      let_stack := [];
      loop body;
      let_stack := [];
      loop handler;
      let_stack := []
    | Utrywith (body, ident, handler) ->
      let_stack := [];
      loop body;
      let_stack := [];
      ignore_ident ident;
      loop handler;
      let_stack := []
    | Uifthenelse (cond, ifso, ifnot) ->
      examine_argument_list [cond];
      let_stack := [];
      loop ifso;
      let_stack := [];
      loop ifnot;
      let_stack := []
    | Usequence (e1, e2) ->
      loop e1;
      let_stack := [];
      loop e2;
      let_stack := []
    | Uwhile (cond, body) ->
      let_stack := [];
      loop cond;
      let_stack := [];
      loop body;
      let_stack := []
    | Ufor (ident, low, high, direction_flag, body) ->
      ignore_ident ident;
      (* Cmmgen generates code that evaluates low before high,
         but we don't do anything here at the moment anyway. *)
      ignore_ulambda low;
      ignore_ulambda high;
      ignore_direction_flag direction_flag;
      let_stack := [];
      loop body;
      let_stack := []
    | Uassign (ident, expr) ->
      ignore_ident ident;
      ignore_ulambda expr;
      let_stack := []
    | Usend (meth_kind, e1, e2, args, dbg) ->
      ignore_meth_kind meth_kind;
      ignore_ulambda e1;
      ignore_ulambda e2;
      ignore_ulambda_list args;
      let_stack := [];
      ignore_debuginfo dbg
    | Uunreachable ->
      let_stack := []
  in
  loop clam;
  !can_move

(* Substitution of an expression for a let-moveable variable can cause the
   surrounding expression to become fixed.  To avoid confusion, do the
   let-moveable substitutions first. *)
let rec substitute_let_moveable is_let_moveable env (clam : Clambda.ulambda)
      : Clambda.ulambda =
  match clam with
  | Uvar id ->
    if not (Ident.Set.mem id is_let_moveable) then
      clam
    else
      begin match Ident.Map.find id env with
      | clam -> clam
      | exception Not_found ->
        Misc.fatal_errorf "substitute_let_moveable: Unbound identifier %a"
          Ident.print id
      end
  | Uconst _ -> clam
  | Udirect_apply (label, args, dbg) ->
    let args = substitute_let_moveable_list is_let_moveable env args in
    Udirect_apply (label, args, dbg)
  | Ugeneric_apply (func, args, dbg) ->
    let func = substitute_let_moveable is_let_moveable env func in
    let args = substitute_let_moveable_list is_let_moveable env args in
    Ugeneric_apply (func, args, dbg)
  | Uclosure (functions, variables_bound_by_the_closure) ->
    let functions =
      List.map (fun (ufunction : Clambda.ufunction) ->
          { ufunction with
            body = substitute_let_moveable is_let_moveable env ufunction.body;
          })
        functions
    in
    let variables_bound_by_the_closure =
      substitute_let_moveable_list is_let_moveable env
        variables_bound_by_the_closure
    in
    Uclosure (functions, variables_bound_by_the_closure)
  | Uoffset (clam, n) ->
    let clam = substitute_let_moveable is_let_moveable env clam in
    Uoffset (clam, n)
  | Ulet (let_kind, value_kind, id, def, body) ->
    let def = substitute_let_moveable is_let_moveable env def in
    if Ident.Set.mem id is_let_moveable then
      let env = Ident.Map.add id def env in
      substitute_let_moveable is_let_moveable env body
    else
      Ulet (let_kind, value_kind,
            id, def, substitute_let_moveable is_let_moveable env body)
  | Uletrec (defs, body) ->
    let defs =
      List.map (fun (id, def) ->
          id, substitute_let_moveable is_let_moveable env def)
        defs
    in
    let body = substitute_let_moveable is_let_moveable env body in
    Uletrec (defs, body)
  | Uprim (prim, args, dbg) ->
    let args = substitute_let_moveable_list is_let_moveable env args in
    Uprim (prim, args, dbg)
  | Uswitch (cond, sw, dbg) ->
    let cond = substitute_let_moveable is_let_moveable env cond in
    let sw =
      { sw with
        us_actions_consts =
          substitute_let_moveable_array is_let_moveable env
            sw.us_actions_consts;
        us_actions_blocks =
          substitute_let_moveable_array is_let_moveable env
            sw.us_actions_blocks;
      }
    in
    Uswitch (cond, sw, dbg)
  | Ustringswitch (cond, branches, default) ->
    let cond = substitute_let_moveable is_let_moveable env cond in
    let branches =
      List.map (fun (s, branch) ->
          s, substitute_let_moveable is_let_moveable env branch)
        branches
    in
    let default =
      Misc.may_map (substitute_let_moveable is_let_moveable env) default
    in
    Ustringswitch (cond, branches, default)
  | Ustaticfail (n, args) ->
    let args = substitute_let_moveable_list is_let_moveable env args in
    Ustaticfail (n, args)
  | Ucatch (n, ids, body, handler) ->
    let body = substitute_let_moveable is_let_moveable env body in
    let handler = substitute_let_moveable is_let_moveable env handler in
    Ucatch (n, ids, body, handler)
  | Utrywith (body, id, handler) ->
    let body = substitute_let_moveable is_let_moveable env body in
    let handler = substitute_let_moveable is_let_moveable env handler in
    Utrywith (body, id, handler)
  | Uifthenelse (cond, ifso, ifnot) ->
    let cond = substitute_let_moveable is_let_moveable env cond in
    let ifso = substitute_let_moveable is_let_moveable env ifso in
    let ifnot = substitute_let_moveable is_let_moveable env ifnot in
    Uifthenelse (cond, ifso, ifnot)
  | Usequence (e1, e2) ->
    let e1 = substitute_let_moveable is_let_moveable env e1 in
    let e2 = substitute_let_moveable is_let_moveable env e2 in
    Usequence (e1, e2)
  | Uwhile (cond, body) ->
    let cond = substitute_let_moveable is_let_moveable env cond in
    let body = substitute_let_moveable is_let_moveable env body in
    Uwhile (cond, body)
  | Ufor (id, low, high, direction, body) ->
    let low = substitute_let_moveable is_let_moveable env low in
    let high = substitute_let_moveable is_let_moveable env high in
    let body = substitute_let_moveable is_let_moveable env body in
    Ufor (id, low, high, direction, body)
  | Uassign (id, expr) ->
    let expr = substitute_let_moveable is_let_moveable env expr in
    Uassign (id, expr)
  | Usend (kind, e1, e2, args, dbg) ->
    let e1 = substitute_let_moveable is_let_moveable env e1 in
    let e2 = substitute_let_moveable is_let_moveable env e2 in
    let args = substitute_let_moveable_list is_let_moveable env args in
    Usend (kind, e1, e2, args, dbg)
  | Uunreachable ->
    Uunreachable

and substitute_let_moveable_list is_let_moveable env clams =
  List.map (substitute_let_moveable is_let_moveable env) clams

and substitute_let_moveable_array is_let_moveable env clams =
  Array.map (substitute_let_moveable is_let_moveable env) clams

(* We say that an expression is "moveable" iff it has neither effects nor
   coeffects.  (See semantics_of_primitives.mli.)
*)
type moveable = Fixed | Constant | Moveable

let both_moveable a b =
  match a, b with
  | Constant, Constant -> Constant
  | Constant, Moveable
  | Moveable, Constant
  | Moveable, Moveable -> Moveable
  | Constant, Fixed
  | Moveable, Fixed
  | Fixed, Constant
  | Fixed, Moveable
  | Fixed, Fixed -> Fixed

let primitive_moveable (prim : Lambda.primitive)
    (args : Clambda.ulambda list)
    (ident_info : ident_info) =
  match prim, args with
  | Pfield _, [Uconst (Uconst_ref (_, _))] ->
    (* CR-someday mshinwell: Actually, maybe this shouldn't be needed; these
       should have been simplified to [Read_symbol_field], which doesn't yield
       a Clambda let.  This might be fixed when Inline_and_simplify can
       turn Pfield into Read_symbol_field. *)
    (* Allow field access of symbols to be moveable.  (The comment in
       flambda.mli on [Read_symbol_field] may be helpful to the reader.) *)
    Moveable
  | Pfield _, [Uvar id] when Ident.Set.mem id ident_info.closure_environment ->
    (* accesses to the function environment is coeffect free: this block
       is never mutated *)
    Moveable
  | _ ->
    match Semantics_of_primitives.for_primitive prim with
    | No_effects, No_coeffects -> Moveable
    | No_effects, Has_coeffects
    | Only_generative_effects, No_coeffects
    | Only_generative_effects, Has_coeffects
    | Arbitrary_effects, No_coeffects
    | Arbitrary_effects, Has_coeffects -> Fixed

type moveable_for_env = Constant | Moveable

(** Eliminate, through substitution, [let]-bindings of linear variables with
    moveable defining expressions. *)
let rec un_anf_and_moveable ident_info env (clam : Clambda.ulambda)
      : Clambda.ulambda * moveable =
  match clam with
  | Uvar id ->
    begin match Ident.Map.find id env with
    | Constant, def -> def, Constant
    | Moveable, def -> def, Moveable
    | exception Not_found ->
      let moveable : moveable =
        if Ident.Set.mem id ident_info.assigned then
          Fixed
        else
          Moveable
      in
      clam, moveable
    end
  | Uconst _ ->
    (* Constant closures are rewritten separately. *)
    clam, Constant
  | Udirect_apply (label, args, dbg) ->
    let args = un_anf_list ident_info env args in
    Udirect_apply (label, args, dbg), Fixed
  | Ugeneric_apply (func, args, dbg) ->
    let func = un_anf ident_info env func in
    let args = un_anf_list ident_info env args in
    Ugeneric_apply (func, args, dbg), Fixed
  | Uclosure (functions, variables_bound_by_the_closure) ->
    let functions =
      List.map (fun (ufunction : Clambda.ufunction) ->
          { ufunction with
            body = un_anf ident_info env ufunction.body;
          })
        functions
    in
    let variables_bound_by_the_closure =
      un_anf_list ident_info env variables_bound_by_the_closure
    in
    Uclosure (functions, variables_bound_by_the_closure), Fixed
  | Uoffset (clam, n) ->
    let clam, moveable = un_anf_and_moveable ident_info env clam in
    Uoffset (clam, n), both_moveable Moveable moveable
  | Ulet (_let_kind, _value_kind, id, def, Uvar id') when Ident.same id id' ->
    un_anf_and_moveable ident_info env def
  | Ulet (let_kind, value_kind, id, def, body) ->
    let def, def_moveable = un_anf_and_moveable ident_info env def in
    let is_linear = Ident.Set.mem id ident_info.linear in
    let is_used = Ident.Set.mem id ident_info.used in
    let is_assigned = Ident.Set.mem id ident_info.assigned in
    begin match def_moveable, is_linear, is_used, is_assigned with
    | (Constant | Moveable), _, false, _ ->
      (* A moveable expression that is never used may be eliminated. *)
      un_anf_and_moveable ident_info env body
    | Constant, _, true, false
    (* A constant expression bound to an unassigned identifier can replace any
         occurrences of the identifier. *)
    | Moveable, true, true, false  ->
      (* A moveable expression bound to a linear unassigned [Ident.t]
         may replace the single occurrence of the identifier. *)
      let def_moveable =
        match def_moveable with
        | Moveable -> Moveable
        | Constant -> Constant
        | Fixed -> assert false
      in
      let env = Ident.Map.add id (def_moveable, def) env in
      un_anf_and_moveable ident_info env body
    | (Constant | Moveable), _, _, true
        (* Constant or Moveable but assigned. *)
    | Moveable, false, _, _
        (* Moveable but not used linearly. *)
    | Fixed, _, _, _ ->
      let body, body_moveable = un_anf_and_moveable ident_info env body in
      Ulet (let_kind, value_kind, id, def, body),
      both_moveable def_moveable body_moveable
    end
  | Uletrec (defs, body) ->
    let defs =
      List.map (fun (id, def) -> id, un_anf ident_info env def) defs
    in
    let body = un_anf ident_info env body in
    Uletrec (defs, body), Fixed
  | Uprim (prim, args, dbg) ->
    let args, args_moveable = un_anf_list_and_moveable ident_info env args in
    let moveable =
      both_moveable args_moveable (primitive_moveable prim args ident_info)
    in
    Uprim (prim, args, dbg), moveable
  | Uswitch (cond, sw, dbg) ->
    let cond = un_anf ident_info env cond in
    let sw =
      { sw with
        us_actions_consts = un_anf_array ident_info env sw.us_actions_consts;
        us_actions_blocks = un_anf_array ident_info env sw.us_actions_blocks;
      }
    in
    Uswitch (cond, sw, dbg), Fixed
  | Ustringswitch (cond, branches, default) ->
    let cond = un_anf ident_info env cond in
    let branches =
      List.map (fun (s, branch) -> s, un_anf ident_info env branch)
        branches
    in
    let default = Misc.may_map (un_anf ident_info env) default in
    Ustringswitch (cond, branches, default), Fixed
  | Ustaticfail (n, args) ->
    let args = un_anf_list ident_info env args in
    Ustaticfail (n, args), Fixed
  | Ucatch (n, ids, body, handler) ->
    let body = un_anf ident_info env body in
    let handler = un_anf ident_info env handler in
    Ucatch (n, ids, body, handler), Fixed
  | Utrywith (body, id, handler) ->
    let body = un_anf ident_info env body in
    let handler = un_anf ident_info env handler in
    Utrywith (body, id, handler), Fixed
  | Uifthenelse (cond, ifso, ifnot) ->
    let cond, cond_moveable = un_anf_and_moveable ident_info env cond in
    let ifso, ifso_moveable = un_anf_and_moveable ident_info env ifso in
    let ifnot, ifnot_moveable = un_anf_and_moveable ident_info env ifnot in
    let moveable =
      both_moveable cond_moveable
        (both_moveable ifso_moveable ifnot_moveable)
    in
    Uifthenelse (cond, ifso, ifnot), moveable
  | Usequence (e1, e2) ->
    let e1 = un_anf ident_info env e1 in
    let e2 = un_anf ident_info env e2 in
    Usequence (e1, e2), Fixed
  | Uwhile (cond, body) ->
    let cond = un_anf ident_info env cond in
    let body = un_anf ident_info env body in
    Uwhile (cond, body), Fixed
  | Ufor (id, low, high, direction, body) ->
    let low = un_anf ident_info env low in
    let high = un_anf ident_info env high in
    let body = un_anf ident_info env body in
    Ufor (id, low, high, direction, body), Fixed
  | Uassign (id, expr) ->
    let expr = un_anf ident_info env expr in
    Uassign (id, expr), Fixed
  | Usend (kind, e1, e2, args, dbg) ->
    let e1 = un_anf ident_info env e1 in
    let e2 = un_anf ident_info env e2 in
    let args = un_anf_list ident_info env args in
    Usend (kind, e1, e2, args, dbg), Fixed
  | Uunreachable ->
    Uunreachable, Fixed

and un_anf ident_info env clam : Clambda.ulambda =
  let clam, _moveable = un_anf_and_moveable ident_info env clam in
  clam

and un_anf_list_and_moveable ident_info env clams
      : Clambda.ulambda list * moveable =
  List.fold_right (fun clam (l, acc_moveable) ->
      let clam, moveable = un_anf_and_moveable ident_info env clam in
      clam :: l, both_moveable moveable acc_moveable)
    clams ([], (Moveable : moveable))

and un_anf_list ident_info env clams : Clambda.ulambda list =
  let clams, _moveable = un_anf_list_and_moveable ident_info env clams in
  clams

and un_anf_array ident_info env clams : Clambda.ulambda array =
  Array.map (un_anf ident_info env) clams

let apply clam ~what =
  let ident_info = make_ident_info clam in
  let let_bound_vars_that_can_be_moved =
    let_bound_vars_that_can_be_moved ident_info clam
  in
  let clam =
    substitute_let_moveable let_bound_vars_that_can_be_moved
      Ident.Map.empty clam
  in
  let ident_info = make_ident_info clam in
  let clam = un_anf ident_info Ident.Map.empty clam in
  if !Clflags.dump_clambda then begin
    Format.eprintf "@.un-anf (%s):@ %a@." what Printclambda.clambda clam
  end;
  clam
