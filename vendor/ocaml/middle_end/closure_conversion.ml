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

module Env = Closure_conversion_aux.Env
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module IdentSet = Lambda.IdentSet

let name_expr = Flambda_utils.name_expr

type t = {
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
  filename : string;
  mutable imported_symbols : Symbol.Set.t;
  mutable declared_symbols : (Symbol.t * Flambda.constant_defining_value) list;
}

let add_default_argument_wrappers lam =
  let defs_are_all_functions (defs : (_ * Lambda.lambda) list) =
    List.for_all (function (_, Lambda.Lfunction _) -> true | _ -> false) defs
  in
  let f (lam : Lambda.lambda) : Lambda.lambda =
    match lam with
    | Llet (( Strict | Alias | StrictOpt), _k, id,
        Lfunction {kind; params; body = fbody; attr; loc}, body) ->
      begin match
        Simplif.split_default_wrapper ~id ~kind ~params
          ~body:fbody ~attr ~loc
      with
      | [fun_id, def] -> Llet (Alias, Pgenval, fun_id, def, body)
      | [fun_id, def; inner_fun_id, def_inner] ->
        Llet (Alias, Pgenval, inner_fun_id, def_inner,
              Llet (Alias, Pgenval, fun_id, def, body))
      | _ -> assert false
      end
    | Lletrec (defs, body) as lam ->
      if defs_are_all_functions defs then
        let defs =
          List.flatten
            (List.map
               (function
                 | (id, Lambda.Lfunction {kind; params; body; attr; loc}) ->
                   Simplif.split_default_wrapper ~id ~kind ~params ~body
                     ~attr ~loc
                 | _ -> assert false)
               defs)
        in
        Lletrec (defs, body)
      else lam
    | lam -> lam
  in
  Lambda.map f lam

(** Generate a wrapper ("stub") function that accepts a tuple argument and
    calls another function with arguments extracted in the obvious
    manner from the tuple. *)
let tupled_function_call_stub original_params unboxed_version
      : Flambda.function_declaration =
  let tuple_param_var =
    Variable.rename ~append:"tupled_stub_param" unboxed_version
  in
  let params = List.map (fun p -> Variable.rename p) original_params in
  let call : Flambda.t =
    Apply ({
        func = unboxed_version;
        args = params;
        (* CR-someday mshinwell for mshinwell: investigate if there is some
           redundancy here (func is also unboxed_version) *)
        kind = Direct (Closure_id.wrap unboxed_version);
        dbg = Debuginfo.none;
        inline = Default_inline;
        specialise = Default_specialise;
      })
  in
  let _, body =
    List.fold_left (fun (pos, body) param ->
        let lam : Flambda.named =
          Prim (Pfield (pos, Fld_na), [tuple_param_var], Debuginfo.none)
        in
        pos + 1, Flambda.create_let param lam body)
      (0, call) params
  in
  let tuple_param = Parameter.wrap tuple_param_var in
  Flambda.create_function_declaration ~params:[tuple_param]
    ~body ~stub:true ~dbg:Debuginfo.none ~inline:Default_inline
    ~specialise:Default_specialise ~is_a_functor:false

let register_const t (constant:Flambda.constant_defining_value) name
      : Flambda.constant_defining_value_block_field * string =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  (* Create a variable to ensure uniqueness of the symbol *)
  let var = Variable.create ~current_compilation_unit name in
  let symbol =
    Symbol.create current_compilation_unit
      (Linkage_name.create (Variable.unique_name var))
  in
  t.declared_symbols <- (symbol, constant) :: t.declared_symbols;
  Symbol symbol, name

let rec declare_const t (const : Lambda.structured_constant)
      : Flambda.constant_defining_value_block_field * string =
  match const with
  | Const_base (Const_int c) -> Const (Int c), "int"
  | Const_base (Const_char c) -> Const (Char c), "char"
  | Const_base (Const_string (s, _)) ->
    let const, name =
      if Config.safe_string then
        Flambda.Allocated_const (Immutable_string s), "immstring"
      else Flambda.Allocated_const (String s), "string"
    in
    register_const t const name
  | Const_base (Const_float c) ->
    register_const t
      (Allocated_const (Float (float_of_string c)))
      "float"
  | Const_base (Const_int32 c) ->
    register_const t (Allocated_const (Int32 c)) "int32"
  | Const_base (Const_int64 c) ->
    register_const t (Allocated_const (Int64 c)) "int64"
  | Const_base (Const_nativeint c) ->
    register_const t (Allocated_const (Nativeint c)) "nativeint"
  | Const_pointer (c,_) -> Const (Const_pointer c), "pointer"
  | Const_immstring c ->
    register_const t (Allocated_const (Immutable_string c)) "immstring"
  | Const_float_array c ->
    register_const t
      (Allocated_const (Immutable_float_array (List.map float_of_string c)))
      "float_array"
  | Const_block (tag, _, consts) ->
    let const : Flambda.constant_defining_value =
      Block (Tag.create_exn tag,
             List.map (fun c -> fst (declare_const t c)) consts)
    in
    register_const t const "const_block"

let close_const t (const : Lambda.structured_constant)
      : Flambda.named * string =
  match declare_const t const with
  | Const c, name ->
    Const c, name
  | Symbol s, name ->
    Symbol s, name

let rec close t env (lam : Lambda.lambda) : Flambda.t =
  match lam with
  | Lvar id ->
    begin match Env.find_var_exn env id with
    | var -> Var var
    | exception Not_found ->
      match Env.find_mutable_var_exn env id with
      | mut_var -> name_expr (Read_mutable mut_var) ~name:"read_mutable"
      | exception Not_found ->
        Misc.fatal_errorf "Closure_conversion.close: unbound identifier %a"
          Ident.print id
    end
  | Lconst cst ->
    let cst, name = close_const t cst in
    name_expr cst ~name:("const_" ^ name)
  | Llet ((Strict | Alias | StrictOpt), _value_kind, id, defining_expr, body) ->
    (* TODO: keep value_kind in flambda *)
    let var = Variable.create_with_same_name_as_ident id in
    let defining_expr =
      close_let_bound_expression t var env defining_expr
    in
    let body = close t (Env.add_var env id var) body in
    Flambda.create_let var defining_expr body
  | Llet (Variable, block_kind, id, defining_expr, body) ->
    let mut_var = Mutable_variable.of_ident id in
    let var = Variable.create_with_same_name_as_ident id in
    let defining_expr =
      close_let_bound_expression t var env defining_expr
    in
    let body = close t (Env.add_mutable_var env id mut_var) body in
    Flambda.create_let var defining_expr
      (Let_mutable
         { var = mut_var;
           initial_value = var;
           body;
           contents_kind = block_kind })
  | Lfunction { kind; params; body; attr; loc; } ->
    let name =
      (* Name anonymous functions by their source location, if known. *)
      if loc = Location.none then "anon-fn"
      else Format.asprintf "anon-fn[%a]" Location.print_compact loc
    in
    let closure_bound_var = Variable.create name in
    (* CR-soon mshinwell: some of this is now very similar to the let rec case
       below *)
    let set_of_closures_var = Variable.create ("set_of_closures_" ^ name) in
    let set_of_closures =
      let decl =
        Function_decl.create ~let_rec_ident:None ~closure_bound_var ~kind
          ~params ~body ~attr ~loc
      in
      close_functions t env (Function_decls.create [decl])
    in
    let project_closure : Flambda.project_closure =
      { set_of_closures = set_of_closures_var;
        closure_id = Closure_id.wrap closure_bound_var;
      }
    in
    Flambda.create_let set_of_closures_var set_of_closures
      (name_expr (Project_closure (project_closure))
        ~name:("project_closure_" ^ name))
  | Lapply { ap_func; ap_args; ap_loc; ap_should_be_tailcall = _;
        ap_inlined; ap_specialised; } ->
    Lift_code.lifting_helper (close_list t env ap_args)
      ~evaluation_order:`Right_to_left
      ~name:"apply_arg"
      ~create_body:(fun args ->
        let func = close t env ap_func in
        let func_var = Variable.create "apply_funct" in
        Flambda.create_let func_var (Expr func)
          (Apply ({
              func = func_var;
              args;
              kind = Indirect;
              dbg = Debuginfo.from_location ap_loc;
              inline = ap_inlined;
              specialise = ap_specialised;
            })))
  | Lletrec (defs, body) ->
    let env =
      List.fold_right (fun (id,  _) env ->
          Env.add_var env id (Variable.create_with_same_name_as_ident id))
        defs env
    in
    let function_declarations =
      (* Identify any bindings in the [let rec] that are functions.  These
         will be named after the corresponding identifier in the [let rec]. *)
      List.map (function
          | (let_rec_ident,
             Lambda.Lfunction { kind; params; body; attr; loc }) ->
            let closure_bound_var =
              Variable.create_with_same_name_as_ident let_rec_ident
            in
            let function_declaration =
              Function_decl.create ~let_rec_ident:(Some let_rec_ident)
                ~closure_bound_var ~kind ~params ~body
                ~attr ~loc
            in
            Some function_declaration
          | _ -> None)
        defs
    in
    begin match
      Misc.Stdlib.List.some_if_all_elements_are_some function_declarations
    with
    | Some function_declarations ->
      (* When all the bindings are (syntactically) functions, we can
         eliminate the [let rec] construction, instead producing a normal
         [Let] that binds a set of closures containing all of the functions.
      *)
      (* CR-someday lwhite: This is a very syntactic criteria. Adding an
         unused value to a set of recursive bindings changes how
         functions are represented at runtime. *)
      let name =
        (* The Microsoft assembler has a 247-character limit on symbol
           names, so we keep them shorter to try not to hit this. *)
        if Sys.win32 then begin
          match defs with
          | (id, _)::_ -> (Ident.unique_name id) ^ "_let_rec"
          | _ -> "let_rec"
        end else begin
          String.concat "_and_"
            (List.map (fun (id, _) -> Ident.unique_name id) defs)
        end
      in
      let set_of_closures_var = Variable.create name in
      let set_of_closures =
        close_functions t env (Function_decls.create function_declarations)
      in
      let body =
        List.fold_left (fun body decl ->
            let let_rec_ident = Function_decl.let_rec_ident decl in
            let closure_bound_var = Function_decl.closure_bound_var decl in
            let let_bound_var = Env.find_var env let_rec_ident in
            (* Inside the body of the [let], each function is referred to by
               a [Project_closure] expression, which projects from the set of
               closures. *)
            (Flambda.create_let let_bound_var
              (Project_closure {
                set_of_closures = set_of_closures_var;
                closure_id = Closure_id.wrap closure_bound_var;
              })
              body))
          (close t env body) function_declarations
      in
      Flambda.create_let set_of_closures_var set_of_closures body
    | None ->
      (* If the condition above is not satisfied, we build a [Let_rec]
         expression; any functions bound by it will have their own
         individual closures. *)
      let defs =
        List.map (fun (id, def) ->
            let var = Env.find_var env id in
            var, close_let_bound_expression t ~let_rec_ident:id var env def)
          defs
      in
      Let_rec (defs, close t env body)
    end
  | Lsend (kind, meth, obj, args, loc) ->
    let meth_var = Variable.create "meth" in
    let obj_var = Variable.create "obj" in
    let dbg = Debuginfo.from_location loc in
    Flambda.create_let meth_var (Expr (close t env meth))
      (Flambda.create_let obj_var (Expr (close t env obj))
        (Lift_code.lifting_helper (close_list t env args)
          ~evaluation_order:`Right_to_left
          ~name:"send_arg"
          ~create_body:(fun args ->
              Send { kind; meth = meth_var; obj = obj_var; args; dbg; })))
  | Lprim ((Pdivint Safe | Pmodint Safe
           | Pdivbint { is_safe = Safe } | Pmodbint { is_safe = Safe }) as prim,
           [arg1; arg2], loc)
      when not !Clflags.fast -> (* not -unsafe *)
    let arg2 = close t env arg2 in
    let arg1 = close t env arg1 in
    let numerator = Variable.create "numerator" in
    let denominator = Variable.create "denominator" in
    let zero = Variable.create "zero" in
    let is_zero = Variable.create "is_zero" in
    let exn = Variable.create "division_by_zero" in
    let exn_symbol =
      t.symbol_for_global' Predef.ident_division_by_zero
    in
    let dbg = Debuginfo.from_location loc in
    let zero_const : Flambda.named =
      match prim with
      | Pdivint _ | Pmodint _ ->
        Const (Int 0)
      | Pdivbint { size = Pint32 } | Pmodbint { size = Pint32 } ->
        Allocated_const (Int32 0l)
      | Pdivbint { size = Pint64 } | Pmodbint { size = Pint64 } ->
        Allocated_const (Int64 0L)
      | Pdivbint { size = Pnativeint } | Pmodbint { size = Pnativeint } ->
        Allocated_const (Nativeint 0n)
      | _ -> assert false
    in
    let prim : Lambda.primitive =
      match prim with
      | Pdivint _ -> Pdivint Unsafe
      | Pmodint _ -> Pmodint Unsafe
      | Pdivbint { size } -> Pdivbint { size; is_safe = Unsafe }
      | Pmodbint { size } -> Pmodbint { size; is_safe = Unsafe }
      | _ -> assert false
    in
    let comparison : Lambda.primitive =
      match prim with
      | Pdivint _ | Pmodint _ -> Pintcomp Ceq
      | Pdivbint { size } | Pmodbint { size } -> Pbintcomp (size,Ceq)
      | _ -> assert false
    in
    t.imported_symbols <- Symbol.Set.add exn_symbol t.imported_symbols;
    Flambda.create_let zero zero_const
      (Flambda.create_let exn (Symbol exn_symbol)
        (Flambda.create_let denominator (Expr arg2)
          (Flambda.create_let numerator (Expr arg1)
            (Flambda.create_let is_zero
              (Prim (comparison, [zero; denominator], dbg))
                (If_then_else (is_zero,
                  name_expr (Prim (Praise Raise_regular, [exn], dbg))
                    ~name:"dummy",
                  (* CR-someday pchambart: find the right event.
                     mshinwell: I briefly looked at this, and couldn't
                     figure it out.
                     lwhite: I don't think any of the existing events
                     are suitable. I had to add a new one for a similar
                     case in the array data types work.
                     mshinwell: deferred CR *)
                  name_expr ~name:"result"
                    (Prim (prim, [numerator; denominator], dbg))))))))
  | Lprim ((Pdivint Safe | Pmodint Safe
           | Pdivbint { is_safe = Safe } | Pmodbint { is_safe = Safe }), _, _)
      when not !Clflags.fast ->
    Misc.fatal_error "Pdivint / Pmodint must have exactly two arguments"
  | Lprim (Psequor, [arg1; arg2], _) ->
    let arg1 = close t env arg1 in
    let arg2 = close t env arg2 in
    let const_true = Variable.create "const_true" in
    let cond = Variable.create "cond_sequor" in
    Flambda.create_let const_true (Const (Const_pointer 1))
      (Flambda.create_let cond (Expr arg1)
        (If_then_else (cond, Var const_true, arg2)))
  | Lprim (Psequand, [arg1; arg2], _) ->
    let arg1 = close t env arg1 in
    let arg2 = close t env arg2 in
    let const_false = Variable.create "const_false" in
    let cond = Variable.create "cond_sequand" in
    Flambda.create_let const_false (Const (Const_pointer 0))
      (Flambda.create_let cond (Expr arg1)
        (If_then_else (cond, arg2, Var const_false)))
  | Lprim ((Psequand | Psequor), _, _) ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  | Lprim (Pidentity, [arg], _) -> close t env arg
  | Lprim (Pdirapply, [funct; arg], loc)
  | Lprim (Prevapply, [arg; funct], loc) ->
    let apply : Lambda.lambda_apply =
      { ap_func = funct;
        ap_args = [arg];
        ap_loc = loc;
        ap_should_be_tailcall = false;
        (* CR-someday lwhite: it would be nice to be able to give
           inlined attributes to functions applied with the application
           operators. *)
        ap_inlined = Default_inline;
        ap_specialised = Default_specialise;
      }
    in
    close t env (Lambda.Lapply apply)
  | Lprim (Praise kind, [arg], loc) ->
    let arg_var = Variable.create "raise_arg" in
    let dbg = Debuginfo.from_location loc in
    Flambda.create_let arg_var (Expr (close t env arg))
      (name_expr
        (Prim (Praise kind, [arg_var], dbg))
        ~name:"raise")
  | Lprim (Pfield _, [Lprim (Pgetglobal id, [],_)], _)
      when Ident.same id t.current_unit_id ->
    Misc.fatal_errorf "[Pfield (Pgetglobal ...)] for the current compilation \
        unit is forbidden upon entry to the middle end"
  | Lprim (Psetfield (_, _, _,_), [Lprim (Pgetglobal _, [], _); _], _) ->
    Misc.fatal_errorf "[Psetfield (Pgetglobal ...)] is \
        forbidden upon entry to the middle end"
  | Lprim (Pgetglobal id, [], _) when Ident.is_predef_exn id ->
    let symbol = t.symbol_for_global' id in
    t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
    name_expr (Symbol symbol) ~name:"predef_exn"
  | Lprim (Pgetglobal id, [], _) ->
    assert (not (Ident.same id t.current_unit_id));
    let symbol = t.symbol_for_global' id in
    t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
    name_expr (Symbol symbol) ~name:"Pgetglobal"
  | Lprim (p, args, loc) ->
    (* One of the important consequences of the ANF-like representation
       here is that we obtain names corresponding to the components of
       blocks being made (with [Pmakeblock]).  This information can be used
       by the simplification pass to increase the likelihood of eliminating
       the allocation, since some field accesses can be tracked back to known
       field values. *)
    let name = Printlambda.name_of_primitive p in
    let dbg = Debuginfo.from_location loc in
    Lift_code.lifting_helper (close_list t env args)
      ~evaluation_order:`Right_to_left
      ~name:(name ^ "_arg")
      ~create_body:(fun args ->
        name_expr (Prim (p, args, dbg))
          ~name)
  | Lswitch (arg, sw, _loc) ->
    let scrutinee = Variable.create "switch" in
    let aux (i, lam) = i, close t env lam in
    let zero_to_n = Numbers.Int.zero_to_n in
    Flambda.create_let scrutinee (Expr (close t env arg))
      (Switch (scrutinee,
        { numconsts = zero_to_n (sw.sw_numconsts - 1);
          consts = List.map aux sw.sw_consts;
          numblocks = zero_to_n (sw.sw_numblocks - 1);
          blocks = List.map aux sw.sw_blocks;
          failaction = Misc.may_map (close t env) sw.sw_failaction;
        }))
  | Lstringswitch (arg, sw, def, _) ->
    let scrutinee = Variable.create "string_switch" in
    Flambda.create_let scrutinee (Expr (close t env arg))
      (String_switch (scrutinee,
        List.map (fun (s, e) -> s, close t env e) sw,
        Misc.may_map (close t env) def))
  | Lstaticraise (i, args) ->
    Lift_code.lifting_helper (close_list t env args)
      ~evaluation_order:`Right_to_left
      ~name:"staticraise_arg"
      ~create_body:(fun args ->
        let static_exn = Env.find_static_exception env i in
        Static_raise (static_exn, args))
  | Lstaticcatch (body, (i, ids), handler) ->
    let st_exn = Static_exception.create () in
    let env = Env.add_static_exception env i st_exn in
    let vars = List.map (Variable.create_with_same_name_as_ident) ids in
    Static_catch (st_exn, vars, close t env body,
      close t (Env.add_vars env ids vars) handler)
  | Ltrywith (body, id, handler) ->
    let var = Variable.create_with_same_name_as_ident id in
    Try_with (close t env body, var, close t (Env.add_var env id var) handler)
  | Lifthenelse (cond, ifso, ifnot) ->
    let cond = close t env cond in
    let cond_var = Variable.create "cond" in
    Flambda.create_let cond_var (Expr cond)
      (If_then_else (cond_var, close t env ifso, close t env ifnot))
  | Lsequence (lam1, lam2) ->
    let var = Variable.create "sequence" in
    let lam1 = Flambda.Expr (close t env lam1) in
    let lam2 = close t env lam2 in
    Flambda.create_let var lam1 lam2
  | Lwhile (cond, body) -> While (close t env cond, close t env body)
  | Lfor (id, lo, hi, direction, body) ->
    let bound_var = Variable.create_with_same_name_as_ident id in
    let from_value = Variable.create "for_from" in
    let to_value = Variable.create "for_to" in
    let body = close t (Env.add_var env id bound_var) body in
    Flambda.create_let from_value (Expr (close t env lo))
      (Flambda.create_let to_value (Expr (close t env hi))
        (For { bound_var; from_value; to_value; direction; body; }))
  | Lassign (id, new_value) ->
    let being_assigned =
      match Env.find_mutable_var_exn env id with
      | being_assigned -> being_assigned
      | exception Not_found ->
        Misc.fatal_errorf "Closure_conversion.close: unbound mutable \
            variable %s in assignment"
          (Ident.unique_name id)
    in
    let new_value_var = Variable.create "new_value" in
    Flambda.create_let new_value_var (Expr (close t env new_value))
      (Assign { being_assigned; new_value = new_value_var; })
  | Levent (lam, _) -> close t env lam
  | Lifused _ ->
    (* [Lifused] is used to mark that this expression should be alive only if
       an identifier is.  Every use should have been removed by
       [Simplif.simplify_lets], either by replacing by the inner expression,
       or by completely removing it (replacing by unit). *)
    Misc.fatal_error "[Lifused] should have been removed by \
        [Simplif.simplify_lets]"

(** Perform closure conversion on a set of function declarations, returning a
    set of closures.  (The set will often only contain a single function;
    the only case where it cannot is for "let rec".) *)
and close_functions t external_env function_declarations : Flambda.named =
  let closure_env_without_parameters =
    Function_decls.closure_env_without_parameters
      external_env function_declarations
  in
  let all_free_idents = Function_decls.all_free_idents function_declarations in
  let close_one_function map decl =
    let body = Function_decl.body decl in
    let loc = Function_decl.loc decl in
    let dbg = Debuginfo.from_location loc in
    let params = Function_decl.params decl in
    (* Create fresh variables for the elements of the closure (cf.
       the comment on [Function_decl.closure_env_without_parameters], above).
       This induces a renaming on [Function_decl.free_idents]; the results of
       that renaming are stored in [free_variables]. *)
    let closure_env =
      List.fold_right (fun id env ->
          Env.add_var env id (Variable.create_with_same_name_as_ident id))
        params closure_env_without_parameters
    in
    (* If the function is the wrapper for a function with an optional
       argument with a default value, make sure it always gets inlined.
       CR-someday pchambart: eta-expansion wrapper for a primitive are
       not marked as stub but certainly should *)
    let stub = Function_decl.stub decl in
    let param_vars = List.map (Env.find_var closure_env) params in
    let params = List.map Parameter.wrap param_vars in
    let closure_bound_var = Function_decl.closure_bound_var decl in
    let body = close t closure_env body in
    let fun_decl =
      Flambda.create_function_declaration ~params ~body ~stub ~dbg
        ~inline:(Function_decl.inline decl)
        ~specialise:(Function_decl.specialise decl)
        ~is_a_functor:(Function_decl.is_a_functor decl)
    in
    match Function_decl.kind decl with
    | Curried -> Variable.Map.add closure_bound_var fun_decl map
    | Tupled ->
      let unboxed_version = Variable.rename closure_bound_var in
      let generic_function_stub =
        tupled_function_call_stub param_vars unboxed_version
      in
      Variable.Map.add unboxed_version fun_decl
        (Variable.Map.add closure_bound_var generic_function_stub map)
  in
  let function_decls =
    Flambda.create_function_declarations
      ~funs:
        (List.fold_left close_one_function Variable.Map.empty
          (Function_decls.to_list function_declarations))
  in
  (* The closed representation of a set of functions is a "set of closures".
     (For avoidance of doubt, the runtime representation of the *whole set* is
     a single block with tag [Closure_tag].) *)
  let set_of_closures =
    let free_vars =
      IdentSet.fold (fun var map ->
          let internal_var =
            Env.find_var closure_env_without_parameters var
          in
          let external_var : Flambda.specialised_to =
            { var = Env.find_var external_env var;
              projection = None;
            }
          in
          Variable.Map.add internal_var external_var map)
        all_free_idents Variable.Map.empty
    in
    Flambda.create_set_of_closures ~function_decls ~free_vars
      ~specialised_args:Variable.Map.empty
      ~direct_call_surrogates:Variable.Map.empty
  in
  Set_of_closures set_of_closures

and close_list t sb l = List.map (close t sb) l

and close_let_bound_expression t ?let_rec_ident let_bound_var env
      (lam : Lambda.lambda) : Flambda.named =
  match lam with
  | Lfunction { kind; params; body; attr; loc; } ->
    (* Ensure that [let] and [let rec]-bound functions have appropriate
       names. *)
    let closure_bound_var = Variable.rename let_bound_var in
    let decl =
      Function_decl.create ~let_rec_ident ~closure_bound_var ~kind ~params
        ~body ~attr ~loc
    in
    let set_of_closures_var =
      Variable.rename let_bound_var ~append:"_set_of_closures"
    in
    let set_of_closures =
      close_functions t env (Function_decls.create [decl])
    in
    let project_closure : Flambda.project_closure =
      { set_of_closures = set_of_closures_var;
        closure_id = Closure_id.wrap closure_bound_var;
      }
    in
    Expr (Flambda.create_let set_of_closures_var set_of_closures
      (name_expr (Project_closure (project_closure))
        ~name:(Variable.unique_name let_bound_var)))
  | lam -> Expr (close t env lam)

let lambda_to_flambda ~backend ~module_ident ~size ~filename lam
      : Flambda.program =
  let lam = add_default_argument_wrappers lam in
  let module Backend = (val backend : Backend_intf.S) in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let t =
    { current_unit_id = Compilation_unit.get_persistent_ident compilation_unit;
      symbol_for_global' = Backend.symbol_for_global';
      filename;
      imported_symbols = Symbol.Set.empty;
      declared_symbols = [];
    }
  in
  let module_symbol = Backend.symbol_for_global' module_ident in
  let block_symbol =
    let linkage_name = Linkage_name.create "module_as_block" in
    Symbol.create compilation_unit linkage_name
  in
  (* The global module block is built by accessing the fields of all the
     introduced symbols. *)
  (* CR-soon mshinwell for mshinwell: Add a comment describing how modules are
     compiled. *)
  let fields =
    Array.init size (fun pos ->
      let pos_str = string_of_int pos in
      let sym_v = Variable.create ("block_symbol_" ^ pos_str) in
      let result_v = Variable.create ("block_symbol_get_" ^ pos_str) in
      let value_v = Variable.create ("block_symbol_get_field_" ^ pos_str) in
      Flambda.create_let
        sym_v (Symbol block_symbol)
         (Flambda.create_let result_v
            (Prim (Pfield (0, Fld_na), [sym_v], Debuginfo.none))
            (Flambda.create_let value_v
              (Prim (Pfield (pos, Fld_na), [result_v], Debuginfo.none))
              (Var value_v))))
  in
  let module_initializer : Flambda.program_body =
    Initialize_symbol (
      block_symbol,
      Tag.create_exn 0,
      [close t Env.empty lam],
      Initialize_symbol (
        module_symbol,
        Tag.create_exn 0,
        Array.to_list fields,
        End module_symbol))
  in
  let program_body =
    List.fold_left
      (fun program_body (symbol, constant) : Flambda.program_body ->
         Let_symbol (symbol, constant, program_body))
      module_initializer
      t.declared_symbols
  in
  { imported_symbols = t.imported_symbols;
    program_body;
  }
