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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* This cannot be done in a single simple pass due to expressions like:

  let rec ... =
    ...
    let rec f1 x =
      let f2 y =
        f1 rec_list
      in
      f2 v
    and rec_list = f1 :: rec_list in
    ...

  and v = ...

  f1, f2 and rec_list are constants iff v is a constant.

  To handle this we populate both a 'not constant' set NC and a set of
  implications between variables.

  For example, the above code would generate the implications:

      f1 in NC => rec_list in NC
      f2 in NC => f1 in NC
      rec_list in NC => f2 in NC
      v in NC => f1 in NC

   then if v is found to be in NC this will be propagated to place
   f1, f2 and rec_list in NC as well.

*)

(* CR-someday lwhite: I think this pass could be combined with
   alias_analysis and other parts of lift_constants into a single
   type-based analysis which infers a "type" for each variable that is
   either an allocated_constant expression or "not constant".  Recursion
   would be handled with unification variables. *)

module Int = Numbers.Int
module Symbol_field = struct
  type t = Symbol.t * Int.t
  include Identifiable.Make (Identifiable.Pair (Symbol) (Int))
end

type dep =
  | Closure of Set_of_closures_id.t
  | Var of Variable.t
  | Symbol of Symbol.t
  | Symbol_field of Symbol_field.t

type state =
  | Not_constant
  | Implication of dep list

type result = {
  id : state Variable.Tbl.t;
  closure : state Set_of_closures_id.Tbl.t;
}

module type Param = sig
  val program : Flambda.program
  val compilation_unit : Compilation_unit.t
end

(* CR-soon mshinwell: consider removing functor *)
module Inconstants (P:Param) (Backend:Backend_intf.S) = struct
  let program = P.program
  let compilation_unit = P.compilation_unit
  let imported_symbols = Flambda_utils.imported_symbols program

  (* Sets representing NC *)
  let variables : state Variable.Tbl.t = Variable.Tbl.create 42
  let closures : state Set_of_closures_id.Tbl.t =
    Set_of_closures_id.Tbl.create 42
  let symbols : state Symbol.Tbl.t = Symbol.Tbl.create 42
  let symbol_fields : state Symbol_field.Tbl.t = Symbol_field.Tbl.create 42

  let mark_queue = Queue.create ()

  (* CR-soon pchambart: We could probably improve that quite a lot by adding
     (the future annotation) [@unrolled] at the right call sites.  Or more
     directly mark mark_dep as [@inline] and call it instead of mark_curr in
     some situations.
  *)

  (* adds 'dep in NC' *)
  let rec mark_dep = function
    | Var id -> begin
      match Variable.Tbl.find variables id with
      | Not_constant -> ()
      | Implication deps ->
        Variable.Tbl.replace variables id Not_constant;
        Queue.push deps mark_queue
      | exception Not_found ->
        Variable.Tbl.add variables id Not_constant
      end
    | Closure cl -> begin
      match Set_of_closures_id.Tbl.find closures cl with
      | Not_constant -> ()
      | Implication deps ->
        Set_of_closures_id.Tbl.replace closures cl Not_constant;
        Queue.push deps mark_queue
      | exception Not_found ->
        Set_of_closures_id.Tbl.add closures cl Not_constant
      end
    | Symbol s -> begin
      match Symbol.Tbl.find symbols s with
      | Not_constant -> ()
      | Implication deps ->
        Symbol.Tbl.replace symbols s Not_constant;
        Queue.push deps mark_queue
      | exception Not_found ->
        Symbol.Tbl.add symbols s Not_constant
      end
    | Symbol_field s -> begin
      match Symbol_field.Tbl.find symbol_fields s with
      | Not_constant -> ()
      | Implication deps ->
        Symbol_field.Tbl.replace symbol_fields s Not_constant;
        Queue.push deps mark_queue
      | exception Not_found ->
        Symbol_field.Tbl.add symbol_fields s Not_constant
      end

  and mark_deps deps =
    List.iter mark_dep deps

  and complete_marking () =
    while not (Queue.is_empty mark_queue) do
      let deps =
        try
          Queue.take mark_queue
        with Not_found -> []
      in
      mark_deps deps;
    done

  (* adds 'curr in NC' *)
  let mark_curr curr =
    mark_deps curr;
    complete_marking ()

  (* adds in the tables 'dep in NC => curr in NC' *)
  let register_implication ~in_nc:dep ~implies_in_nc:curr =
    match dep with
    | Var id -> begin
      match Variable.Tbl.find variables id with
      | Not_constant ->
        mark_deps curr;
        complete_marking ();
      | Implication deps ->
        let deps = List.rev_append curr deps in
        Variable.Tbl.replace variables id (Implication deps)
      | exception Not_found ->
        Variable.Tbl.add variables id (Implication curr);
      end
    | Closure cl -> begin
      match Set_of_closures_id.Tbl.find closures cl with
      | Not_constant ->
        mark_deps curr;
        complete_marking ();
      | Implication deps ->
        let deps = List.rev_append curr deps in
        Set_of_closures_id.Tbl.replace closures cl (Implication deps)
      | exception Not_found ->
        Set_of_closures_id.Tbl.add closures cl (Implication curr);
      end
    | Symbol symbol -> begin
      match Symbol.Tbl.find symbols symbol with
      | Not_constant ->
        mark_deps curr;
        complete_marking ();
      | Implication deps ->
        let deps = List.rev_append curr deps in
        Symbol.Tbl.replace symbols symbol (Implication deps)
      | exception Not_found ->
        Symbol.Tbl.add symbols symbol (Implication curr);
      end
    | Symbol_field ((symbol, _) as field) -> begin
      match Symbol_field.Tbl.find symbol_fields field with
      | Not_constant ->
        mark_deps curr;
        complete_marking ();
      | Implication deps ->
        let deps = List.rev_append curr deps in
        Symbol_field.Tbl.replace symbol_fields field (Implication deps)
      | exception Not_found ->
        (* There is no information available about the contents of imported
           symbols, so we must consider all their fields as inconstant. *)
        (* CR-someday pchambart: recover that from the cmx information *)
        if Symbol.Set.mem symbol imported_symbols then begin
          Symbol_field.Tbl.add symbol_fields field Not_constant;
          mark_deps curr;
          complete_marking ();
        end else begin
          Symbol_field.Tbl.add symbol_fields field (Implication curr)
        end
      end

  (* First loop: iterates on the tree to mark dependencies.

     curr is the variables or closures to which we add constraints like
     '... in NC => curr in NC' or 'curr in NC'

     It can be empty when no constraint can be added like in the toplevel
     expression or in the body of a function.
  *)
  let rec mark_loop ~toplevel (curr : dep list) (flam : Flambda.t) =
    match flam with
    | Let { var; defining_expr = lam; body; _ } ->
      mark_named ~toplevel [Var var] lam;
      (* adds 'var in NC => curr in NC'
         This is not really necessary, but compiling this correctly is
         trickier than eliminating that earlier. *)
      mark_var var curr;
      mark_loop ~toplevel curr body
    | Let_mutable { initial_value = var; body } ->
      mark_var var curr;
      mark_loop ~toplevel curr body
    | Let_rec(defs, body) ->
      List.iter (fun (var, def) ->
          mark_named ~toplevel [Var var] def;
          (* adds 'var in NC => curr in NC' same remark as let case *)
          mark_var var curr)
        defs;
      mark_loop ~toplevel curr body
    | Var var -> mark_var var curr
    (* Not constant cases: we mark directly 'curr in NC' and mark
       bound variables as in NC also *)
    | Assign _ ->
      mark_curr curr
    | Try_with (f1,id,f2) ->
      mark_curr [Var id];
      mark_curr curr;
      mark_loop ~toplevel [] f1;
      mark_loop ~toplevel [] f2
    | Static_catch (_,ids,f1,f2) ->
      List.iter (fun id -> mark_curr [Var id]) ids;
      mark_curr curr;
      mark_loop ~toplevel [] f1;
      mark_loop ~toplevel [] f2
      (* CR-someday pchambart: If recursive staticcatch is introduced:
         this becomes ~toplevel:false *)
    | For { bound_var; from_value; to_value; direction = _; body; } ->
      mark_curr [Var bound_var];
      mark_var from_value curr;
      mark_var to_value curr;
      mark_curr curr;
      mark_loop ~toplevel:false [] body
    | While (f1,body) ->
      mark_curr curr;
      mark_loop ~toplevel [] f1;
      mark_loop ~toplevel:false [] body
    | If_then_else (f1,f2,f3) ->
      mark_curr curr;
      mark_curr [Var f1];
      mark_loop ~toplevel [] f2;
      mark_loop ~toplevel [] f3
    | Static_raise (_,l) ->
      mark_curr curr;
      List.iter (fun v -> mark_var v curr) l
    | Apply ({func; args; _ }) ->
      mark_curr curr;
      mark_var func curr;
      mark_vars args curr;
    | Switch (arg,sw) ->
      mark_curr curr;
      mark_var arg curr;
      List.iter (fun (_,l) -> mark_loop ~toplevel [] l) sw.consts;
      List.iter (fun (_,l) -> mark_loop ~toplevel [] l) sw.blocks;
      Misc.may (fun l -> mark_loop ~toplevel [] l) sw.failaction
    | String_switch (arg,sw,def) ->
      mark_curr curr;
      mark_var arg curr;
      List.iter (fun (_,l) -> mark_loop ~toplevel [] l) sw;
      Misc.may (fun l -> mark_loop ~toplevel [] l) def
    | Send { kind = _; meth; obj; args; dbg = _; } ->
      mark_curr curr;
      mark_var meth curr;
      mark_var obj curr;
      List.iter (fun arg -> mark_var arg curr) args
    | Proved_unreachable ->
      mark_curr curr

  and mark_named ~toplevel curr (named : Flambda.named) =
    match named with
    | Set_of_closures (set_of_closures) ->
      mark_loop_set_of_closures ~toplevel curr set_of_closures
    | Const _ | Allocated_const _ -> ()
    | Read_mutable _ -> mark_curr curr
    | Symbol symbol -> begin
        let current_unit = Compilation_unit.get_current_exn () in
        if Compilation_unit.equal current_unit (Symbol.compilation_unit symbol)
        then
          ()
        else
          match (Backend.import_symbol symbol).descr with
          | Value_unresolved _ ->
            (* Constant when 'for_clambda' means: can be a symbol (which is
               obviously the case here) with a known approximation.  If this
               condition is not satisfied we mark as inconstant to reflect
               the fact that the symbol's contents are unknown and thus
               prevent attempts to examine it.  (This is a bit of a hack.) *)
            mark_curr curr
          | _ ->
            ()
      end
    | Read_symbol_field (symbol, index) ->
      register_implication ~in_nc:(Symbol_field (symbol, index))
        ~implies_in_nc:curr
    (* Globals are symbols: handle like symbols *)
    | Prim (Lambda.Pgetglobal _id, [], _) -> ()
    (* Constant constructors: those expressions are constant if all their
       parameters are:
       - makeblock is compiled to a constant block
       - offset is compiled to a pointer inside a constant closure.
         See Cmmgen for the details

       makeblock(Mutable) can be a 'constant' if it is allocated at
       toplevel: if this expression is evaluated only once.
    *)
    | Prim (Lambda.Pmakeblock (_tag, _, Asttypes.Immutable, _value_kind), args,
            _dbg) ->
      mark_vars args curr
(*  (* CR-someday pchambart: If global mutables are allowed: *)
    | Prim(Lambda.Pmakeblock(_tag, Asttypes.Mutable), args, _dbg, _)
      when toplevel ->
      List.iter (mark_loop ~toplevel curr) args
*)
    | Prim (Pmakearray (Pfloatarray, Immutable), args, _) ->
      mark_vars args curr
    | Prim (Pmakearray (Pfloatarray, Mutable), args, _) ->
      (* CR-someday pchambart: Toplevel float arrays could always be
         statically allocated using an equivalent of the
         Initialize_symbol construction.
         Toplevel non-float arrays could also be turned into an
         Initialize_symbol, but only when declared as immutable since
         preallocated symbols does not allow mutation after
         initialisation
      *)
      if toplevel then mark_vars args curr
      else mark_curr curr
    | Prim (Pduparray (Pfloatarray, Immutable), [arg], _) ->
      mark_var arg curr
    | Prim (Pduparray (Pfloatarray, Mutable), [arg], _) ->
      if toplevel then mark_var arg curr
      else mark_curr curr
    | Prim (Pduparray _, _, _) ->
      (* See Lift_constants *)
      mark_curr curr
    | Project_closure ({ set_of_closures; closure_id; }) ->
      if Closure_id.in_compilation_unit closure_id compilation_unit then
        mark_var set_of_closures curr
      else
        mark_curr curr
    | Move_within_set_of_closures ({ closure; start_from; move_to; }) ->
      (* CR-someday mshinwell: We should be able to deem these projections
         (same for the cases below) as constant when from another
         compilation unit, but there isn't code to handle this yet.  (Note
         that for Project_var we cannot yet generate a projection from a
         closure in another compilation unit, since we only lift closed
         closures.) *)
      if Closure_id.in_compilation_unit start_from compilation_unit then begin
        assert (Closure_id.in_compilation_unit move_to compilation_unit);
        mark_var closure curr
      end else begin
        mark_curr curr
      end
    | Project_var ({ closure; closure_id; var = _ }) ->
      if Closure_id.in_compilation_unit closure_id compilation_unit then
        mark_var closure curr
      else
        mark_curr curr
    | Prim (Lambda.Pfield _, [f1], _) ->
      mark_curr curr;
      mark_var f1 curr
    | Prim (_, args, _) ->
      mark_curr curr;
      mark_vars args curr
    | Expr flam ->
      mark_loop ~toplevel curr flam

  and mark_var var curr =
    (* adds 'id in NC => curr in NC' *)
    register_implication ~in_nc:(Var var) ~implies_in_nc:curr

  and mark_vars vars curr =
    (* adds 'id in NC => curr in NC' *)
    List.iter (fun var -> mark_var var curr) vars

  (* [toplevel] is intended for allowing static allocations of mutable
     blocks.  This feature should be available in a future release once the
     necessary GC changes have been merged. (See GPR#178.) *)
  and mark_loop_set_of_closures ~toplevel:_ curr
        { Flambda. function_decls; free_vars; specialised_args } =
    (* If a function in the set of closures is specialised, do not consider
       it constant, unless all specialised args are also constant. *)
    Variable.Map.iter (fun _ (spec_arg : Flambda.specialised_to) ->
          register_implication
            ~in_nc:(Var spec_arg.var)
            ~implies_in_nc:[Closure function_decls.set_of_closures_id])
        specialised_args;
    (* adds 'function_decls in NC => curr in NC' *)
    register_implication ~in_nc:(Closure function_decls.set_of_closures_id)
      ~implies_in_nc:curr;
    (* a closure is constant if its free variables are constants. *)
    Variable.Map.iter (fun inner_id (var : Flambda.specialised_to) ->
        register_implication ~in_nc:(Var var.var)
          ~implies_in_nc:[
            Var inner_id;
            Closure function_decls.set_of_closures_id
          ])
      free_vars;
    Variable.Map.iter (fun fun_id (ffunc : Flambda.function_declaration) ->
        (* for each function f in a closure c 'c in NC => f' *)
        register_implication ~in_nc:(Closure function_decls.set_of_closures_id)
          ~implies_in_nc:[Var fun_id];
        (* function parameters are in NC unless specialised *)
        List.iter (fun param ->
            match Variable.Map.find param specialised_args with
            | exception Not_found -> mark_curr [Var param]
            | outer_var ->
              register_implication ~in_nc:(Var outer_var.var)
                ~implies_in_nc:[Var param])
          (Parameter.List.vars ffunc.params);
        mark_loop ~toplevel:false [] ffunc.body)
      function_decls.funs

  let mark_constant_defining_value (const:Flambda.constant_defining_value) =
    match const with
    | Allocated_const _
    | Block _
    | Project_closure _ -> ()
    | Set_of_closures set_of_closure ->
      mark_loop_set_of_closures ~toplevel:true [] set_of_closure

  let mark_program (program : Flambda.program) =
    let rec loop (program : Flambda.program_body) =
      match program with
      | End _ -> ()
      | Initialize_symbol (symbol,_tag,fields,program) ->
        List.iteri (fun i field ->
            mark_loop ~toplevel:true
              [Symbol symbol; Symbol_field (symbol,i)] field)
          fields;
        loop program
      | Effect (expr, program) ->
        mark_loop ~toplevel:true [] expr;
        loop program
      | Let_symbol (_, def, program) ->
        mark_constant_defining_value def;
        loop program
      | Let_rec_symbol (defs, program) ->
        List.iter (fun (_, def) -> mark_constant_defining_value def) defs;
        loop program
    in
    loop program.program_body

  let res =
    mark_program program;
    { id = variables;
      closure = closures;
    }
end

let inconstants_on_program ~compilation_unit ~backend
    (program : Flambda.program) =
  let module P = struct
    let program = program
    let compilation_unit = compilation_unit
  end in
  let module Backend = (val backend : Backend_intf.S) in
  let module I = Inconstants (P) (Backend) in
  I.res

let variable var { id; _ } =
  match Variable.Tbl.find id var with
  | Not_constant -> true
  | Implication _ -> false
  | exception Not_found -> false

let closure cl { closure; _ } =
  match Set_of_closures_id.Tbl.find closure cl with
  | Not_constant -> true
  | Implication _ -> false
  | exception Not_found -> false
