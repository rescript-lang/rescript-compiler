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

module A = Simple_value_approx
module B = Inlining_cost.Benefit
module E = Inline_and_simplify_aux.Env
module R = Inline_and_simplify_aux.Result

(** Values of two types hold the information propagated during simplification:
    - [E.t] "environments", top-down, almost always called "env";
    - [R.t] "results", bottom-up approximately following the evaluation order,
      almost always called "r".  These results come along with rewritten
      Flambda terms.
    The environments map variables to approximations, which enable various
    simplifications to be performed; for example, some variable may be known
    to always hold a particular constant.
*)

let ret = R.set_approx

type simplify_variable_result =
  | No_binding of Variable.t
  | Binding of Variable.t * (Flambda.named Flambda.With_free_variables.t)

let simplify_free_variable_internal env original_var =
  let var = Freshening.apply_variable (E.freshening env) original_var in
  let original_var = var in
  (* In the case where an approximation is useful, we introduce a [let]
     to bind (e.g.) the constant or symbol replacing [var], unless this
     would introduce a useless [let] as a consequence of [var] already being
     in the current scope.

     Even when the approximation is not useful, this simplification helps.
     In particular, it squashes aliases of the form:
      let var1 = var2 in ... var2 ...
     by replacing [var2] in the body with [var1].  Simplification can then
     eliminate the [let].
  *)
  let var =
    let approx = E.find_exn env var in
    match approx.var with
    | Some var when E.mem env var -> var
    | Some _ | None -> var
  in
  (* CR-soon mshinwell: Should we update [r] when we *add* code?
     Aside from that, it looks like maybe we don't need [r] in this function,
     because the approximation within it wouldn't be used by any of the
     call sites. *)
  match E.find_with_scope_exn env var with
  | Current, approx -> No_binding var, approx  (* avoid useless [let] *)
  | Outer, approx ->
    match A.simplify_var approx with
    | None -> No_binding var, approx
    | Some (named, approx) ->
      let module W = Flambda.With_free_variables in
      Binding (original_var, W.of_named named), approx

let simplify_free_variable env var ~f : Flambda.t * R.t =
  match simplify_free_variable_internal env var with
  | No_binding var, approx -> f env var approx
  | Binding (var, named), approx ->
    let module W = Flambda.With_free_variables in
    let var = Variable.rename var in
    let env = E.add env var approx in
    let body, r = f env var approx in
    (W.create_let_reusing_defining_expr var named body), r

let simplify_free_variables env vars ~f : Flambda.t * R.t =
  let rec collect_bindings vars env bound_vars approxs : Flambda.t * R.t =
    match vars with
    | [] -> f env (List.rev bound_vars) (List.rev approxs)
    | var::vars ->
      match simplify_free_variable_internal env var with
      | No_binding var, approx ->
        collect_bindings vars env (var::bound_vars) (approx::approxs)
      | Binding (var, named), approx ->
        let module W = Flambda.With_free_variables in
        let var = Variable.rename var in
        let env = E.add env var approx in
        let body, r =
          collect_bindings vars env (var::bound_vars) (approx::approxs)
        in
        (W.create_let_reusing_defining_expr var named body), r
  in
  collect_bindings vars env [] []

let simplify_free_variables_named env vars ~f : Flambda.named * R.t =
  let rec collect_bindings vars env bound_vars approxs
        : Flambda.maybe_named * R.t =
    match vars with
    | [] ->
      let named, r = f env (List.rev bound_vars) (List.rev approxs) in
      Is_named named, r
    | var::vars ->
      match simplify_free_variable_internal env var with
      | No_binding var, approx ->
        collect_bindings vars env (var::bound_vars) (approx::approxs)
      | Binding (var, named), approx ->
        let module W = Flambda.With_free_variables in
        let var = Variable.rename var in
        let env = E.add env var approx in
        let body, r =
          collect_bindings vars env (var::bound_vars) (approx::approxs)
        in
        let body =
          match body with
          | Is_named body -> Flambda_utils.name_expr body ~name:"simplify_fv"
          | Is_expr body -> body
        in
        Is_expr (W.create_let_reusing_defining_expr var named body), r
  in
  let named_or_expr, r = collect_bindings vars env [] [] in
  match named_or_expr with
  | Is_named named -> named, r
  | Is_expr expr -> Expr expr, r

(* CR-soon mshinwell: tidy this up *)
let simplify_free_variable_named env var ~f : Flambda.named * R.t =
  simplify_free_variables_named env [var] ~f:(fun env vars vars_approxs ->
    match vars, vars_approxs with
    | [var], [approx] -> f env var approx
    | _ -> assert false)

let simplify_named_using_approx r lam approx =
  let lam, _summary, approx = A.simplify_named approx lam in
  lam, R.set_approx r approx

let simplify_using_approx_and_env env r original_lam approx =
  let lam, summary, approx =
    A.simplify_using_env approx ~is_present_in_env:(E.mem env) original_lam
  in
  let r =
    let r = ret r approx in
    match summary with
    (* CR-soon mshinwell: Why is [r] not updated with the cost of adding the
       new code?
       mshinwell: similar to CR above *)
    | Replaced_term -> R.map_benefit r (B.remove_code original_lam)
    | Nothing_done -> r
  in
  lam, r

let simplify_named_using_approx_and_env env r original_named approx =
  let named, summary, approx =
    A.simplify_named_using_env approx ~is_present_in_env:(E.mem env)
      original_named
  in
  let r =
    let r = ret r approx in
    match summary with
    | Replaced_term -> R.map_benefit r (B.remove_code_named original_named)
    | Nothing_done -> r
  in
  named, r

let simplify_const (const : Flambda.const) =
  match const with
  | Int i -> A.value_int i
  | Char c -> A.value_char c
  | Const_pointer i -> A.value_constptr i

let approx_for_allocated_const (const : Allocated_const.t) =
  match const with
  | String s -> A.value_string (String.length s) None
  | Immutable_string s -> A.value_string (String.length s) (Some s)
  | Int32 i -> A.value_boxed_int Int32 i
  | Int64 i -> A.value_boxed_int Int64 i
  | Nativeint i -> A.value_boxed_int Nativeint i
  | Float f -> A.value_float f
  | Float_array a -> A.value_mutable_float_array ~size:(List.length a)
  | Immutable_float_array a ->
      A.value_immutable_float_array
        (Array.map A.value_float (Array.of_list a))

type filtered_switch_branches =
  | Must_be_taken of Flambda.t
  | Can_be_taken of (int * Flambda.t) list

(* Determine whether a given closure ID corresponds directly to a variable
   (bound to a closure) in the given environment.  This happens when the body
   of a [let rec]-bound function refers to another in the same set of closures.
   If we succeed in this process, we can change [Project_closure]
   expressions into [Var] expressions, thus sharing closure projections. *)
let reference_recursive_function_directly env closure_id =
  let closure_id = Closure_id.unwrap closure_id in
  match E.find_opt env closure_id with
  | None -> None
  | Some approx -> Some (Flambda.Expr (Var closure_id), approx)

(* Simplify an expression that takes a set of closures and projects an
   individual closure from it. *)
let simplify_project_closure env r ~(project_closure : Flambda.project_closure)
      : Flambda.named * R.t =
  simplify_free_variable_named env project_closure.set_of_closures
    ~f:(fun _env set_of_closures set_of_closures_approx ->
    match A.check_approx_for_set_of_closures set_of_closures_approx with
    | Wrong ->
      Misc.fatal_errorf "Wrong approximation when projecting closure: %a"
        Flambda.print_project_closure project_closure
    | Unresolved value ->
      (* A set of closures coming from another compilation unit, whose .cmx is
         missing; as such, we cannot have rewritten the function and don't
         need to do any freshening. *)
      Project_closure {
        set_of_closures;
        closure_id = project_closure.closure_id;
      }, ret r (A.value_unresolved value)
    | Unknown ->
      (* CR-soon mshinwell: see CR comment in e.g. simple_value_approx.ml
         [check_approx_for_closure_allowing_unresolved] *)
      Project_closure {
        set_of_closures;
        closure_id = project_closure.closure_id;
      }, ret r (A.value_unknown Other)
    | Unknown_because_of_unresolved_value value ->
      Project_closure {
        set_of_closures;
        closure_id = project_closure.closure_id;
      }, ret r (A.value_unknown (Unresolved_value value))
    | Ok (set_of_closures_var, value_set_of_closures) ->
      let closure_id =
        A.freshen_and_check_closure_id value_set_of_closures
          project_closure.closure_id
      in
      let projecting_from =
        match set_of_closures_var with
        | None -> None
        | Some set_of_closures_var ->
          let projection : Projection.t =
            Project_closure {
              set_of_closures = set_of_closures_var;
              closure_id;
            }
          in
          match E.find_projection env ~projection with
          | None -> None
          | Some var -> Some (var, projection)
      in
      match projecting_from with
      | Some (var, projection) ->
        simplify_free_variable_named env var ~f:(fun _env var var_approx ->
          let r = R.map_benefit r (B.remove_projection projection) in
          Expr (Var var), ret r var_approx)
      | None ->
        match reference_recursive_function_directly env closure_id with
        | Some (flam, approx) -> flam, ret r approx
        | None ->
          let set_of_closures_var =
            match set_of_closures_var with
            | Some set_of_closures_var' when E.mem env set_of_closures_var' ->
              set_of_closures_var
            | Some _ | None -> None
          in
          let approx =
            A.value_closure ?set_of_closures_var value_set_of_closures
              closure_id
          in
          Project_closure { set_of_closures; closure_id; }, ret r approx)

(* Simplify an expression that, given one closure within some set of
   closures, returns another closure (possibly the same one) within the
   same set. *)
let simplify_move_within_set_of_closures env r
      ~(move_within_set_of_closures : Flambda.move_within_set_of_closures)
      : Flambda.named * R.t =
  simplify_free_variable_named env move_within_set_of_closures.closure
    ~f:(fun _env closure closure_approx ->
    match A.check_approx_for_closure_allowing_unresolved closure_approx with
    | Wrong ->
      Misc.fatal_errorf "Wrong approximation when moving within set of \
          closures.  Approximation: %a  Term: %a"
        A.print closure_approx
        Flambda.print_move_within_set_of_closures move_within_set_of_closures
    | Unresolved sym ->
      Move_within_set_of_closures {
          closure;
          start_from = move_within_set_of_closures.start_from;
          move_to = move_within_set_of_closures.move_to;
        },
        ret r (A.value_unresolved sym)
    | Unknown ->
      Move_within_set_of_closures {
          closure;
          start_from = move_within_set_of_closures.start_from;
          move_to = move_within_set_of_closures.move_to;
        },
        ret r (A.value_unknown Other)
    | Unknown_because_of_unresolved_value value ->
      (* For example: a move upon a (move upon a closure whose .cmx file
         is missing). *)
      Move_within_set_of_closures {
          closure;
          start_from = move_within_set_of_closures.start_from;
          move_to = move_within_set_of_closures.move_to;
        },
        ret r (A.value_unknown (Unresolved_value value))
    | Ok (_value_closure, set_of_closures_var, set_of_closures_symbol,
          value_set_of_closures) ->
      let freshen =
        (* CR-soon mshinwell: potentially misleading name---not freshening with
           new names, but with previously fresh names *)
        A.freshen_and_check_closure_id value_set_of_closures
      in
      let move_to = freshen move_within_set_of_closures.move_to in
      let start_from = freshen move_within_set_of_closures.start_from in
      let projection : Projection.t =
        Move_within_set_of_closures {
          closure;
          start_from;
          move_to;
        }
      in
      match E.find_projection env ~projection with
      | Some var ->
        simplify_free_variable_named env var ~f:(fun _env var var_approx ->
          let r = R.map_benefit r (B.remove_projection projection) in
          Expr (Var var), ret r var_approx)
      | None ->
        match reference_recursive_function_directly env move_to with
        | Some (flam, approx) -> flam, ret r approx
        | None ->
          if Closure_id.equal start_from move_to then
            (* Moving from one closure to itself is a no-op.  We can return an
               [Var] since we already have a variable bound to the closure. *)
            Expr (Var closure), ret r closure_approx
          else
            match set_of_closures_var with
            | Some set_of_closures_var when E.mem env set_of_closures_var ->
              (* A variable bound to the set of closures is in scope,
                 meaning we can rewrite the [Move_within_set_of_closures] to a
                 [Project_closure]. *)
              let project_closure : Flambda.project_closure =
                { set_of_closures = set_of_closures_var;
                  closure_id = move_to;
                }
              in
              let approx =
                A.value_closure ~set_of_closures_var value_set_of_closures
                  move_to
              in
              Project_closure project_closure, ret r approx
            | Some _ | None ->
              match set_of_closures_symbol with
              | Some set_of_closures_symbol ->
                let set_of_closures_var = Variable.create "symbol" in
                let project_closure : Flambda.project_closure =
                  { set_of_closures = set_of_closures_var;
                    closure_id = move_to;
                  }
                in
                let project_closure_var = Variable.create "project_closure" in
                let let1 =
                  Flambda.create_let project_closure_var
                    (Project_closure project_closure)
                    (Var project_closure_var)
                in
                let expr =
                  Flambda.create_let set_of_closures_var
                    (Symbol set_of_closures_symbol)
                    let1
                in
                let approx =
                  A.value_closure ~set_of_closures_var ~set_of_closures_symbol
                    value_set_of_closures move_to
                in
                Expr expr, ret r approx
              | None ->
                (* The set of closures is not available in scope, and we
                   have no other information by which to simplify the move. *)
                let move_within : Flambda.move_within_set_of_closures =
                  { closure; start_from; move_to; }
                in
                let approx = A.value_closure value_set_of_closures move_to in
                Move_within_set_of_closures move_within, ret r approx)

(* Transform an expression denoting an access to a variable bound in
   a closure.  Variables in the closure ([project_var.closure]) may
   have been freshened since [expr] was constructed; as such, we
   must ensure the same happens to [expr].  The renaming information is
   contained within the approximation deduced from [closure] (as
   such, that approximation *must* identify which closure it is).

   For instance in some imaginary syntax for flambda:

     [let f x =
        let g y ~closure:{a} = a + y in
        let closure = { a = x } in
          g 12 ~closure]

   when [f] is traversed, [g] can be inlined, resulting in the
   expression

     [let f z =
        let g y ~closure:{a} = a + y in
        let closure = { a = x } in
          closure.a + 12]

   [closure.a] being a notation for:

     [Project_var{closure = closure; closure_id = g; var = a}]

   If [f] is inlined later, the resulting code will be

     [let x = ... in
      let g' y' ~closure':{a'} = a' + y' in
      let closure' = { a' = x } in
        closure'.a' + 12]

   in particular the field [a] of the closure has been alpha renamed to [a'].
   This information must be carried from the declaration to the use.

   If the function is declared outside of the alpha renamed part, there is
   no need for renaming in the [Ffunction] and [Project_var].
   This is not usually the case, except when the closure declaration is a
   symbol.

   What ensures that this information is available at [Project_var]
   point is that those constructions can only be introduced by inlining,
   which requires that same information. For this to still be valid,
   other transformation must avoid transforming the information flow in
   a way that the inline function can't propagate it.
*)
let rec simplify_project_var env r ~(project_var : Flambda.project_var)
      : Flambda.named * R.t =
  simplify_free_variable_named env project_var.closure
    ~f:(fun _env closure approx ->
    match A.check_approx_for_closure_allowing_unresolved approx with
    | Ok (value_closure, _set_of_closures_var, _set_of_closures_symbol,
          value_set_of_closures) ->
      let module F = Freshening.Project_var in
      let freshening = value_set_of_closures.freshening in
      let var = F.apply_var_within_closure freshening project_var.var in
      let closure_id = F.apply_closure_id freshening project_var.closure_id in
      let closure_id_in_approx = value_closure.closure_id in
      if not (Closure_id.equal closure_id closure_id_in_approx) then begin
        Misc.fatal_errorf "When simplifying [Project_var], the closure ID %a \
            in the approximation of the set of closures did not match the \
            closure ID %a in the [Project_var] term.  Approximation: %a@. \
            Var-within-closure being projected: %a@."
          Closure_id.print closure_id_in_approx
          Closure_id.print closure_id
          Simple_value_approx.print approx
          Var_within_closure.print var
      end;
      let projection : Projection.t =
        Project_var {
          closure;
          closure_id;
          var;
        }
      in
      begin match E.find_projection env ~projection with
      | Some var ->
        simplify_free_variable_named env var ~f:(fun _env var var_approx ->
          let r = R.map_benefit r (B.remove_projection projection) in
          Expr (Var var), ret r var_approx)
      | None ->
        let approx = A.approx_for_bound_var value_set_of_closures var in
        let expr : Flambda.named = Project_var { closure; closure_id; var; } in
        let unwrapped = Var_within_closure.unwrap var in
        let expr =
          if E.mem env unwrapped then
            Flambda.Expr (Var unwrapped)
          else
            expr
        in
        simplify_named_using_approx_and_env env r expr approx
      end
    | Unresolved symbol ->
      (* This value comes from a symbol for which we couldn't find any
         approximation, telling us that names within the closure couldn't
         have been renamed.  So we don't need to change the variable or
         closure ID in the [Project_var] expression. *)
      Project_var { project_var with closure },
        ret r (A.value_unresolved symbol)
    | Unknown ->
      Project_var { project_var with closure },
        ret r (A.value_unknown Other)
    | Unknown_because_of_unresolved_value value ->
      Project_var { project_var with closure },
        ret r (A.value_unknown (Unresolved_value value))
    | Wrong ->
      (* We must have the correct approximation of the value to ensure
         we take account of all freshenings. *)
      Misc.fatal_errorf "[Project_var] from a value with wrong \
          approximation: %a@.closure=%a@.approx of closure=%a@."
        Flambda.print_project_var project_var
        Variable.print closure
        Simple_value_approx.print approx)

(* Transforms closure definitions by applying [loop] on the code of every
   one of the set and on the expressions of the free variables.
   If the substitution is activated, alpha renaming also occur on everything
   defined by the set of closures:
   * Variables bound by a closure of the set
   * closure identifiers
   * parameters

   The rewriting occurs in a clean environment without any of the variables
   defined outside reachable.  This helps increase robustness against
   accidental, potentially unsound simplification of variable accesses by
   [simplify_using_approx_and_env].

   The rewriting occurs in an environment filled with:
   * The approximation of the free variables
   * An explicitely unknown approximation for function parameters,
     except for those where it is known to be safe: those present in the
     [specialised_args] set.
   * An approximation for the closures in the set. It contains the code of
     the functions before rewriting.

   The approximation of the currently defined closures is available to
   allow marking recursives calls as direct and in some cases, allow
   inlining of one closure from the set inside another one. For this to
   be correct an alpha renaming is first applied on the expressions by
   [apply_function_decls_and_free_vars].

   For instance when rewriting the declaration

     [let rec f_1 x_1 =
        let y_1 = x_1 + 1 in
        g_1 y_1
      and g_1 z_1 = f_1 (f_1 z_1)]

   When rewriting this function, the first substitution will contain
   some mapping:
   { f_1 -> f_2;
     g_1 -> g_2;
     x_1 -> x_2;
     z_1 -> z_2 }

   And the approximation for the closure will contain

   { f_2:
       fun x_2 ->
         let y_1 = x_2 + 1 in
         g_2 y_1
     g_2:
       fun z_2 -> f_2 (f_2 z_2) }

   Note that no substitution is applied to the let-bound variable [y_1].
   If [f_2] where to be inlined inside [g_2], we known that a new substitution
   will be introduced in the current scope for [y_1] each time.


   If the function where a recursive one coming from another compilation
   unit, the code already went through [Flambdasym] that could have
   replaced the function variable by the symbol identifying the function
   (this occur if the function contains only constants in its closure).
   To handle that case, we first replace those symbols by the original
   variable.
*)
and simplify_set_of_closures original_env r
      (set_of_closures : Flambda.set_of_closures)
      : Flambda.set_of_closures * R.t * Freshening.Project_var.t =
  let function_decls =
    let module Backend = (val (E.backend original_env) : Backend_intf.S) in
    (* CR-soon mshinwell: Does this affect
       [reference_recursive_function_directly]?
       mshinwell: This should be thought about as part of the wider issue of
       references to functions via symbols or variables. *)
    Freshening.rewrite_recursive_calls_with_symbols (E.freshening original_env)
      set_of_closures.function_decls
      ~make_closure_symbol:Backend.closure_symbol
  in
  let env = E.increase_closure_depth original_env in
  let free_vars, specialised_args, function_decls, parameter_approximations,
      internal_value_set_of_closures, set_of_closures_env =
    Inline_and_simplify_aux.prepare_to_simplify_set_of_closures ~env
      ~set_of_closures ~function_decls ~only_for_function_decl:None
      ~freshen:true
  in
  let simplify_function fun_var (function_decl : Flambda.function_declaration)
        (funs, used_params, r)
        : Flambda.function_declaration Variable.Map.t * Variable.Set.t * R.t =
    let closure_env =
      Inline_and_simplify_aux.prepare_to_simplify_closure ~function_decl
        ~free_vars ~specialised_args ~parameter_approximations
        ~set_of_closures_env
    in
    let body, r =
      E.enter_closure closure_env ~closure_id:(Closure_id.wrap fun_var)
        ~inline_inside:
          (Inlining_decision.should_inline_inside_declaration function_decl)
        ~dbg:function_decl.dbg
        ~f:(fun body_env -> simplify body_env r function_decl.body)
    in
    let inline : Lambda.inline_attribute =
      match function_decl.inline with
      | Default_inline ->
        if !Clflags.classic_inlining && not function_decl.stub then
          (* In classic-inlining mode, the inlining decision is taken at
             definition site (here). If the function is small enough
             (below the -inline threshold) it will always be inlined. *)
          let inlining_threshold =
            Inline_and_simplify_aux.initial_inlining_threshold
              ~round:(E.round env)
          in
          if Inlining_cost.can_inline body inlining_threshold ~bonus:0
          then
            Always_inline
          else
            Default_inline
        else
          Default_inline
      | inline ->
        inline
    in
    let function_decl =
      Flambda.create_function_declaration ~params:function_decl.params
        ~body ~stub:function_decl.stub ~dbg:function_decl.dbg
        ~inline ~specialise:function_decl.specialise
        ~is_a_functor:function_decl.is_a_functor
    in
    let used_params' = Flambda.used_params function_decl in
    Variable.Map.add fun_var function_decl funs,
      Variable.Set.union used_params used_params', r
  in
  let funs, _used_params, r =
    Variable.Map.fold simplify_function function_decls.funs
      (Variable.Map.empty, Variable.Set.empty, r)
  in
  let function_decls =
    Flambda.update_function_declarations function_decls ~funs
  in
  let invariant_params =
    lazy (Invariant_params.invariant_params_in_recursion function_decls
      ~backend:(E.backend env))
  in
  let value_set_of_closures =
    A.create_value_set_of_closures ~function_decls
      ~bound_vars:internal_value_set_of_closures.bound_vars
      ~invariant_params
      ~specialised_args:internal_value_set_of_closures.specialised_args
      ~freshening:internal_value_set_of_closures.freshening
      ~direct_call_surrogates:
        internal_value_set_of_closures.direct_call_surrogates
  in
  let direct_call_surrogates =
    Closure_id.Map.fold (fun existing surrogate surrogates ->
        Variable.Map.add (Closure_id.unwrap existing)
          (Closure_id.unwrap surrogate) surrogates)
      internal_value_set_of_closures.direct_call_surrogates
      Variable.Map.empty
  in
  let set_of_closures =
    Flambda.create_set_of_closures ~function_decls
      ~free_vars:(Variable.Map.map fst free_vars)
      ~specialised_args
      ~direct_call_surrogates
  in
  let r = ret r (A.value_set_of_closures value_set_of_closures) in
  set_of_closures, r, value_set_of_closures.freshening

and simplify_apply env r ~(apply : Flambda.apply) : Flambda.t * R.t =
  let {
    Flambda. func = lhs_of_application; args; kind = _; dbg;
    inline = inline_requested; specialise = specialise_requested;
  } = apply in
  let dbg = E.add_inlined_debuginfo env ~dbg in
  simplify_free_variable env lhs_of_application
    ~f:(fun env lhs_of_application lhs_of_application_approx ->
      simplify_free_variables env args ~f:(fun env args args_approxs ->
        (* By using the approximation of the left-hand side of the
           application, attempt to determine which function is being applied
           (even if the application is currently [Indirect]).  If
           successful---in which case we then have a direct
           application---consider inlining. *)
        match A.check_approx_for_closure lhs_of_application_approx with
        | Ok (value_closure, set_of_closures_var,
              set_of_closures_symbol, value_set_of_closures) ->
          let lhs_of_application, closure_id_being_applied,
                value_set_of_closures, env, wrap =
            let closure_id_being_applied = value_closure.closure_id in
            (* If the call site is a direct call to a function that has a
               "direct call surrogate" (see inline_and_simplify_aux.mli),
               repoint the call to the surrogate. *)
            let surrogates = value_set_of_closures.direct_call_surrogates in
            match Closure_id.Map.find closure_id_being_applied surrogates with
            | exception Not_found ->
              lhs_of_application, closure_id_being_applied,
                value_set_of_closures, env, (fun expr -> expr)
            | surrogate ->
              let rec find_transitively surrogate =
                match Closure_id.Map.find surrogate surrogates with
                | exception Not_found -> surrogate
                | surrogate -> find_transitively surrogate
              in
              let surrogate = find_transitively surrogate in
              let surrogate_var =
                Variable.rename lhs_of_application ~append:"_surrogate"
              in
              let move_to_surrogate : Projection.move_within_set_of_closures =
                { closure = lhs_of_application;
                  start_from = closure_id_being_applied;
                  move_to = surrogate;
                }
              in
              let approx_for_surrogate =
                A.value_closure ~closure_var:surrogate_var
                  ?set_of_closures_var ?set_of_closures_symbol
                  value_set_of_closures surrogate
              in
              let env = E.add env surrogate_var approx_for_surrogate in
              let wrap expr =
                Flambda.create_let surrogate_var
                  (Move_within_set_of_closures move_to_surrogate)
                  expr
              in
              surrogate_var, surrogate, value_set_of_closures, env, wrap
          in
          let function_decls = value_set_of_closures.function_decls in
          let function_decl =
            try
              Flambda_utils.find_declaration closure_id_being_applied
                function_decls
            with
            | Not_found ->
              Misc.fatal_errorf "When handling application expression, \
                  approximation references non-existent closure %a@."
                Closure_id.print closure_id_being_applied
          in
          let r =
            match apply.kind with
            | Indirect ->
              R.map_benefit r Inlining_cost.Benefit.direct_call_of_indirect
            | Direct _ -> r
          in
          let nargs = List.length args in
          let arity = Flambda_utils.function_arity function_decl in
          let result, r =
            if nargs = arity then
              simplify_full_application env r ~function_decls
                ~lhs_of_application ~closure_id_being_applied ~function_decl
                ~value_set_of_closures ~args ~args_approxs ~dbg
                ~inline_requested ~specialise_requested
            else if nargs > arity then
              simplify_over_application env r ~args ~args_approxs
                ~function_decls ~lhs_of_application ~closure_id_being_applied
                ~function_decl ~value_set_of_closures ~dbg ~inline_requested
                ~specialise_requested
            else if nargs > 0 && nargs < arity then
              simplify_partial_application env r ~lhs_of_application
                ~closure_id_being_applied ~function_decl ~args ~dbg
                ~inline_requested ~specialise_requested
            else
              Misc.fatal_errorf "Function with arity %d when simplifying \
                  application expression: %a"
                arity Flambda.print (Flambda.Apply apply)
          in
          wrap result, r
        | Wrong ->  (* Insufficient approximation information to simplify. *)
          Apply ({ func = lhs_of_application; args; kind = Indirect; dbg;
              inline = inline_requested; specialise = specialise_requested; }),
            ret r (A.value_unknown Other)))

and simplify_full_application env r ~function_decls ~lhs_of_application
      ~closure_id_being_applied ~function_decl ~value_set_of_closures ~args
      ~args_approxs ~dbg ~inline_requested ~specialise_requested =
  Inlining_decision.for_call_site ~env ~r ~function_decls
    ~lhs_of_application ~closure_id_being_applied ~function_decl
    ~value_set_of_closures ~args ~args_approxs ~dbg ~simplify
    ~inline_requested ~specialise_requested

and simplify_partial_application env r ~lhs_of_application
      ~closure_id_being_applied ~function_decl ~args ~dbg
      ~inline_requested ~specialise_requested =
  let arity = Flambda_utils.function_arity function_decl in
  assert (arity > List.length args);
  (* For simplicity, we disallow [@inline] attributes on partial
     applications.  The user may always write an explicit wrapper instead
     with such an attribute. *)
  (* CR-someday mshinwell: Pierre noted that we might like a function to be
     inlined when applied to its first set of arguments, e.g. for some kind
     of type class like thing. *)
  begin match (inline_requested : Lambda.inline_attribute) with
  | Always_inline | Never_inline ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@inlined] attributes may not be used \
        on partial applications")
  | Unroll _ ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@unroll] attributes may not be used \
        on partial applications")
  | Default_inline -> ()
  end;
  begin match (specialise_requested : Lambda.specialise_attribute) with
  | Always_specialise | Never_specialise ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@specialised] attributes may not be used \
        on partial applications")
  | Default_specialise -> ()
  end;
  let freshened_params =
    List.map (fun p -> Parameter.rename p) function_decl.Flambda.params
  in
  let applied_args, remaining_args =
    Misc.Stdlib.List.map2_prefix (fun arg id' -> id', arg)
      args freshened_params
  in
  let wrapper_accepting_remaining_args =
    let body : Flambda.t =
      Apply {
        func = lhs_of_application;
        args = Parameter.List.vars freshened_params;
        kind = Direct closure_id_being_applied;
        dbg;
        inline = Default_inline;
        specialise = Default_specialise;
      }
    in
    let closure_variable =
      Variable.rename
        ~append:"_partial_fun"
        (Closure_id.unwrap closure_id_being_applied)
    in
    Flambda_utils.make_closure_declaration ~id:closure_variable
      ~body
      ~params:remaining_args
      ~stub:true
  in
  let with_known_args =
    Flambda_utils.bind
      ~bindings:(List.map (fun (param, arg) ->
          Parameter.var param, Flambda.Expr (Var arg)) applied_args)
      ~body:wrapper_accepting_remaining_args
  in
  simplify env r with_known_args

and simplify_over_application env r ~args ~args_approxs ~function_decls
      ~lhs_of_application ~closure_id_being_applied ~function_decl
      ~value_set_of_closures ~dbg ~inline_requested ~specialise_requested =
  let arity = Flambda_utils.function_arity function_decl in
  assert (arity < List.length args);
  assert (List.length args = List.length args_approxs);
  let full_app_args, remaining_args =
    Misc.Stdlib.List.split_at arity args
  in
  let full_app_approxs, _ =
    Misc.Stdlib.List.split_at arity args_approxs
  in
  let expr, r =
    simplify_full_application env r ~function_decls ~lhs_of_application
      ~closure_id_being_applied ~function_decl ~value_set_of_closures
      ~args:full_app_args ~args_approxs:full_app_approxs ~dbg
      ~inline_requested ~specialise_requested
  in
  let func_var = Variable.create "full_apply" in
  let expr : Flambda.t =
    Flambda.create_let func_var (Expr expr)
      (Apply { func = func_var; args = remaining_args; kind = Indirect; dbg;
        inline = inline_requested; specialise = specialise_requested; })
  in
  let expr = Lift_code.lift_lets_expr expr ~toplevel:true in
  simplify (E.set_never_inline env) r expr

and simplify_named env r (tree : Flambda.named) : Flambda.named * R.t =
  match tree with
  | Symbol sym ->
    (* New Symbol construction could have been introduced during
       transformation (by simplify_named_using_approx_and_env).
       When this comes from another compilation unit, we must load it. *)
    let approx = E.find_or_load_symbol env sym in
    simplify_named_using_approx r tree approx
  | Const cst -> tree, ret r (simplify_const cst)
  | Allocated_const cst -> tree, ret r (approx_for_allocated_const cst)
  | Read_mutable mut_var ->
    (* See comment on the [Assign] case. *)
    let mut_var =
      Freshening.apply_mutable_variable (E.freshening env) mut_var
    in
    Read_mutable mut_var, ret r (A.value_unknown Other)
  | Read_symbol_field (symbol, field_index) ->
    let approx = E.find_or_load_symbol env symbol in
    begin match A.get_field approx ~field_index with
    (* CR-someday mshinwell: Think about [Unreachable] vs. [Value_bottom]. *)
    | Unreachable -> (Flambda.Expr Proved_unreachable), r
    | Ok approx ->
      let approx = A.augment_with_symbol_field approx symbol field_index in
      simplify_named_using_approx_and_env env r tree approx
    end
  | Set_of_closures set_of_closures -> begin
    let backend = E.backend env in
    let set_of_closures, r, first_freshening =
      simplify_set_of_closures env r set_of_closures
    in
    let simplify env r expr ~pass_name : Flambda.named * R.t =
      (* If simplifying a set of closures more than once during any given round
         of simplification, the [Freshening.Project_var] substitutions arising
         from each call to [simplify_set_of_closures] must be composed.
         Note that this function only composes with [first_freshening] owing
         to the structure of the code below (this new [simplify] is always
         in tail position). *)
      (* CR-someday mshinwell: It was mooted that maybe we could try
         structurally-typed closures (i.e. where we would never rename the
         closure elements), or something else, to try to remove
         the "closure freshening" thing in the approximation which is hard
         to deal with. *)
      let expr, r = simplify (E.set_never_inline env) r expr in
      let approx = R.approx r in
      let value_set_of_closures =
        match A.strict_check_approx_for_set_of_closures approx with
        | Wrong ->
          Misc.fatal_errorf "Unexpected approximation returned from \
              simplification of [%s] result: %a"
            pass_name A.print approx
        | Ok (_var, value_set_of_closures) ->
          let freshening =
            Freshening.Project_var.compose ~earlier:first_freshening
              ~later:value_set_of_closures.freshening
          in
          A.update_freshening_of_value_set_of_closures value_set_of_closures
            ~freshening
      in
      Expr expr, (ret r (A.value_set_of_closures value_set_of_closures))
    in
    (* This does the actual substitutions of specialised args introduced
       by [Unbox_closures] for free variables.  (Apart from simplifying
       the [Unbox_closures] output, this also prevents applying
       [Unbox_closures] over and over.) *)
    let set_of_closures =
      match Remove_free_vars_equal_to_args.run set_of_closures with
      | None -> set_of_closures
      | Some set_of_closures -> set_of_closures
    in
    (* Do [Unbox_closures] next to try to decide which things are
       free variables and which things are specialised arguments before
       unboxing them. *)
    match
      Unbox_closures.rewrite_set_of_closures ~env
        ~duplicate_function ~set_of_closures
    with
    | Some (expr, benefit) ->
      let r = R.add_benefit r benefit in
      simplify env r expr ~pass_name:"Unbox_closures"
    | None ->
      match Unbox_free_vars_of_closures.run ~env ~set_of_closures with
      | Some (expr, benefit) ->
        let r = R.add_benefit r benefit in
        simplify env r expr ~pass_name:"Unbox_free_vars_of_closures"
      | None ->
        (* CR-soon mshinwell: should maybe add one allocation for the stub *)
        match
          Unbox_specialised_args.rewrite_set_of_closures ~env
            ~duplicate_function ~set_of_closures
        with
        | Some (expr, benefit) ->
          let r = R.add_benefit r benefit in
          simplify env r expr ~pass_name:"Unbox_specialised_args"
        | None ->
          match
            Remove_unused_arguments.
                separate_unused_arguments_in_set_of_closures
              set_of_closures ~backend
          with
          | Some set_of_closures ->
            let expr =
              Flambda_utils.name_expr (Set_of_closures set_of_closures)
                ~name:"remove_unused_arguments"
            in
            simplify env r expr ~pass_name:"Remove_unused_arguments"
          | None ->
            Set_of_closures set_of_closures, r
    end
  | Project_closure project_closure ->
    simplify_project_closure env r ~project_closure
  | Project_var project_var -> simplify_project_var env r ~project_var
  | Move_within_set_of_closures move_within_set_of_closures ->
    simplify_move_within_set_of_closures env r ~move_within_set_of_closures
  | Prim (prim, args, dbg) ->
    let dbg = E.add_inlined_debuginfo env ~dbg in
    simplify_free_variables_named env args ~f:(fun env args args_approxs ->
      let tree = Flambda.Prim (prim, args, dbg) in
      begin match prim, args, args_approxs with
      | Pgetglobal _, _, _ ->
        Misc.fatal_error "Pgetglobal is forbidden in Inline_and_simplify"
      (* CR-someday mshinwell: Optimise [Pfield_computed]. *)
      | Pfield (field_index,_), [arg], [arg_approx] ->
        let projection : Projection.t = Field (field_index, arg) in
        begin match E.find_projection env ~projection with
        | Some var ->
          simplify_free_variable_named env var ~f:(fun _env var var_approx ->
            let r = R.map_benefit r (B.remove_projection projection) in
            Expr (Var var), ret r var_approx)
        | None ->
          begin match A.get_field arg_approx ~field_index with
          | Unreachable -> (Flambda.Expr Proved_unreachable, r)
          | Ok approx ->
            let tree, approx =
              match arg_approx.symbol with
              (* If the [Pfield] is projecting directly from a symbol, rewrite
                 the expression to [Read_symbol_field]. *)
              | Some (symbol, None) ->
                let approx =
                  A.augment_with_symbol_field approx symbol field_index
                in
                Flambda.Read_symbol_field (symbol, field_index), approx
              | None | Some (_, Some _ ) ->
                (* This [Pfield] is either not projecting from a symbol at all,
                   or it is the projection of a projection from a symbol. *)
                let approx' = E.really_import_approx env approx in
                tree, approx'
            in
            simplify_named_using_approx_and_env env r tree approx
          end
        end
      | Pfield _, _, _ -> Misc.fatal_error "Pfield arity error"
      | (Parraysetu kind | Parraysets kind),
        [_block; _field; _value],
        [block_approx; _field_approx; value_approx] ->
        if A.warn_on_mutation block_approx then begin
          Location.prerr_warning (Debuginfo.to_location dbg)
            Warnings.Assignment_to_non_mutable_value
        end;
        let kind =
          let check () =
            match kind with
            | Pfloatarray | Pgenarray -> ()
            | Paddrarray | Pintarray ->
              (* CR pchambart: Do a proper warning here *)
              Misc.fatal_errorf "Assignment of a float to a specialised \
                                 non-float array: %a"
                Flambda.print_named tree
          in
          match A.descr block_approx, A.descr value_approx with
          | (Value_float_array _, _) -> check (); Lambda.Pfloatarray
          | (_, Value_float _) when Config.flat_float_array ->
            check (); Lambda.Pfloatarray
            (* CR pchambart: This should be accounted by the benefit *)
          | _ ->
            kind
        in
        let prim : Lambda.primitive = match prim with
          | Parraysetu _ -> Parraysetu kind
          | Parraysets _ -> Parraysets kind
          | _ -> assert false
        in
        Prim (prim, args, dbg), ret r (A.value_unknown Other)
      | Psetfield _, _block::_, block_approx::_ ->
        if A.warn_on_mutation block_approx then begin
          Location.prerr_warning (Debuginfo.to_location dbg)
            Warnings.Assignment_to_non_mutable_value
        end;
        tree, ret r (A.value_unknown Other)
      | (Psetfield _ | Parraysetu _ | Parraysets _), _, _ ->
        Misc.fatal_error "Psetfield / Parraysetu / Parraysets arity error"
      | (Psequand | Psequor), _, _ ->
        Misc.fatal_error "Psequand and Psequor must be expanded (see handling \
            in closure_conversion.ml)"
      | p, args, args_approxs ->
        let expr, approx, benefit =
          let module Backend = (val (E.backend env) : Backend_intf.S) in
          Simplify_primitives.primitive p (args, args_approxs) tree dbg
            ~size_int:Backend.size_int ~big_endian:Backend.big_endian
        in
        let r = R.map_benefit r (B.(+) benefit) in
        let approx =
          match p with
          | Popaque -> A.value_unknown Other
          | _ -> approx
        in
        expr, ret r approx
      end)
  | Expr expr ->
    let expr, r = simplify env r expr in
    Expr expr, r

and simplify env r (tree : Flambda.t) : Flambda.t * R.t =
  match tree with
  | Var var ->
    let var = Freshening.apply_variable (E.freshening env) var in
    (* If from the approximations we can simplify [var], then we will be
       forced to insert [let]-expressions (done using [name_expr], in
       [Simple_value_approx]) to bind a [named].  This has an important
       consequence: it brings bindings of constants closer to their use
       points. *)
    simplify_using_approx_and_env env r (Var var) (E.find_exn env var)
  | Apply apply ->
    simplify_apply env r ~apply
  | Let _ ->
    let for_defining_expr (env, r) var defining_expr =
      let defining_expr, r = simplify_named env r defining_expr in
      let var, sb = Freshening.add_variable (E.freshening env) var in
      let env = E.set_freshening env sb in
      let env = E.add env var (R.approx r) in
      (env, r), var, defining_expr
    in
    let for_last_body (env, r) body =
      simplify env r body
    in
    let filter_defining_expr r var defining_expr free_vars_of_body =
      if Variable.Set.mem var free_vars_of_body then
        r, var, Some defining_expr
      else if Effect_analysis.no_effects_named defining_expr then
        let r = R.map_benefit r (B.remove_code_named defining_expr) in
        r, var, None
      else
        r, var, Some defining_expr
    in
    Flambda.fold_lets_option tree
      ~init:(env, r)
      ~for_defining_expr
      ~for_last_body
      ~filter_defining_expr
  | Let_mutable { var = mut_var; initial_value = var; body; contents_kind } ->
    (* CR-someday mshinwell: add the dead let elimination, as above. *)
    simplify_free_variable env var ~f:(fun env var _var_approx ->
      let mut_var, sb =
        Freshening.add_mutable_variable (E.freshening env) mut_var
      in
      let env = E.set_freshening env sb in
      let body, r =
        simplify (E.add_mutable env mut_var (A.value_unknown Other)) r body
      in
      Flambda.Let_mutable
        { var = mut_var;
          initial_value = var;
          body;
          contents_kind },
      r)
  | Let_rec (defs, body) ->
    let defs, sb = Freshening.add_variables (E.freshening env) defs in
    let env = E.set_freshening env sb in
    let def_env =
      List.fold_left (fun env_acc (id, _lam) ->
          E.add env_acc id (A.value_unknown Other))
        env defs
    in
    let defs, body_env, r =
      List.fold_right (fun (id, lam) (defs, env_acc, r) ->
          let lam, r = simplify_named def_env r lam in
          let defs = (id, lam) :: defs in
          let env_acc = E.add env_acc id (R.approx r) in
          defs, env_acc, r)
        defs ([], env, r)
    in
    let body, r = simplify body_env r body in
    Let_rec (defs, body), r
  | Static_raise (i, args) ->
    let i = Freshening.apply_static_exception (E.freshening env) i in
    simplify_free_variables env args ~f:(fun _env args _args_approxs ->
      let r = R.use_static_exception r i in
      Static_raise (i, args), ret r A.value_bottom)
  | Static_catch (i, vars, body, handler) ->
    begin
      match body with
      | Let { var; defining_expr = def; body; _ }
          when not (Flambda_utils.might_raise_static_exn def i) ->
        simplify env r
          (Flambda.create_let var def (Static_catch (i, vars, body, handler)))
      | _ ->
        let i, sb = Freshening.add_static_exception (E.freshening env) i in
        let env = E.set_freshening env sb in
        let body, r = simplify env r body in
        (* CR-soon mshinwell: for robustness, R.used_static_exceptions should
           maybe be removed. *)
        if not (Static_exception.Set.mem i (R.used_static_exceptions r)) then
          (* If the static exception is not used, we can drop the declaration *)
          body, r
        else begin
          match (body : Flambda.t) with
          | Static_raise (j, args) ->
            assert (Static_exception.equal i j);
            let handler =
              List.fold_left2 (fun body var arg ->
                  Flambda.create_let var (Expr (Var arg)) body)
                handler vars args
            in
            let r = R.exit_scope_catch r i in
            simplify env r handler
          | _ ->
            let vars, sb = Freshening.add_variables' (E.freshening env) vars in
            let approx = R.approx r in
            let env =
              List.fold_left (fun env id ->
                  E.add env id (A.value_unknown Other))
                (E.set_freshening env sb) vars
            in
            let env = E.inside_branch env in
            let handler, r = simplify env r handler in
            let r = R.exit_scope_catch r i in
            Static_catch (i, vars, body, handler),
              R.meet_approx r env approx
        end
    end
  | Try_with (body, id, handler) ->
    let body, r = simplify env r body in
    let id, sb = Freshening.add_variable (E.freshening env) id in
    let env = E.add (E.set_freshening env sb) id (A.value_unknown Other) in
    let env = E.inside_branch env in
    let handler, r = simplify env r handler in
    Try_with (body, id, handler), ret r (A.value_unknown Other)
  | If_then_else (arg, ifso, ifnot) ->
    (* When arg is the constant false or true (or something considered
       as true), we can drop the if and replace it by a sequence.
       if arg is not effectful we can also drop it. *)
    simplify_free_variable env arg ~f:(fun env arg arg_approx ->
      begin match arg_approx.descr with
      | Value_constptr 0 | Value_int 0 ->  (* Constant [false]: keep [ifnot] *)
        let ifnot, r = simplify env r ifnot in
        ifnot, R.map_benefit r B.remove_branch
      | Value_constptr _ | Value_int _
      | Value_block _ ->  (* Constant [true]: keep [ifso] *)
        let ifso, r = simplify env r ifso in
        ifso, R.map_benefit r B.remove_branch
      | _ ->
        let env = E.inside_branch env in
        let ifso, r = simplify env r ifso in
        let ifso_approx = R.approx r in
        let ifnot, r = simplify env r ifnot in
        If_then_else (arg, ifso, ifnot),
          R.meet_approx r env ifso_approx
      end)
  | While (cond, body) ->
    let cond, r = simplify env r cond in
    let body, r = simplify env r body in
    While (cond, body), ret r (A.value_unknown Other)
  | Send { kind; meth; obj; args; dbg; } ->
    let dbg = E.add_inlined_debuginfo env ~dbg in
    simplify_free_variable env meth ~f:(fun env meth _meth_approx ->
      simplify_free_variable env obj ~f:(fun env obj _obj_approx ->
        simplify_free_variables env args ~f:(fun _env args _args_approx ->
          Send { kind; meth; obj; args; dbg; },
            ret r (A.value_unknown Other))))
  | For { bound_var; from_value; to_value; direction; body; } ->
    simplify_free_variable env from_value ~f:(fun env from_value _approx ->
      simplify_free_variable env to_value ~f:(fun env to_value _approx ->
        let bound_var, sb =
          Freshening.add_variable (E.freshening env) bound_var
        in
        let env =
          E.add (E.set_freshening env sb) bound_var
            (A.value_unknown Other)
        in
        let body, r = simplify env r body in
        For { bound_var; from_value; to_value; direction; body; },
          ret r (A.value_unknown Other)))
  | Assign { being_assigned; new_value; } ->
    (* No need to use something like [simplify_free_variable]: the
       approximation of [being_assigned] is always unknown. *)
    let being_assigned =
      Freshening.apply_mutable_variable (E.freshening env) being_assigned
    in
    simplify_free_variable env new_value ~f:(fun _env new_value _approx ->
      Assign { being_assigned; new_value; }, ret r (A.value_unknown Other))
  | Switch (arg, sw) ->
    (* When [arg] is known to be a variable whose approximation is that of a
       block with a fixed tag or a fixed integer, we can eliminate the
       [Switch].  (This should also make the [Let] that binds [arg] redundant,
       meaning that it too can be eliminated.) *)
    simplify_free_variable env arg ~f:(fun env arg arg_approx ->
      let rec filter_branches filter branches compatible_branches =
        match branches with
        | [] -> Can_be_taken compatible_branches
        | (c, lam) as branch :: branches ->
          match filter arg_approx c with
          | A.Cannot_be_taken ->
            filter_branches filter branches compatible_branches
          | A.Can_be_taken ->
            filter_branches filter branches (branch :: compatible_branches)
          | A.Must_be_taken ->
            Must_be_taken lam
      in
      let filtered_consts =
        filter_branches A.potentially_taken_const_switch_branch sw.consts []
      in
      let filtered_blocks =
        filter_branches A.potentially_taken_block_switch_branch sw.blocks []
      in
      begin match filtered_consts, filtered_blocks with
      | Must_be_taken _, Must_be_taken _ ->
        assert false
      | Must_be_taken branch, _
      | _, Must_be_taken branch ->
        let lam, r = simplify env r branch in
        lam, R.map_benefit r B.remove_branch
      | Can_be_taken consts, Can_be_taken blocks ->
        match consts, blocks, sw.failaction with
        | [], [], None ->
        (* If the switch is applied to a statically-known value that does not
           match any case:
           * if there is a default action take that case;
           * otherwise this is something that is guaranteed not to
             be reachable by the type checker.  For example:
             [type 'a t = Int : int -> int t | Float : float -> float t
              match Int 1 with
              | Int _ -> ...
              | Float f as v ->
                match v with   <-- This match is unreachable
                | Float f -> ...]
         *)
          Proved_unreachable, ret r A.value_bottom
        | [_, branch], [], None
        | [], [_, branch], None
        | [], [], Some branch ->
          let lam, r = simplify env r branch in
          lam, R.map_benefit r B.remove_branch
        | _ ->
          let env = E.inside_branch env in
          let f (i, v) (acc, r) =
            let approx = R.approx r in
            let lam, r = simplify env r v in
            (i, lam)::acc,
            R.meet_approx r env approx
          in
          let r = R.set_approx r A.value_bottom in
          let consts, r = List.fold_right f consts ([], r) in
          let blocks, r = List.fold_right f blocks ([], r) in
          let failaction, r =
            match sw.failaction with
            | None -> None, r
            | Some l ->
              let approx = R.approx r in
              let l, r = simplify env r l in
              Some l,
              R.meet_approx r env approx
          in
          let sw = { sw with failaction; consts; blocks; } in
          Switch (arg, sw), r
      end)
  | String_switch (arg, sw, def) ->
    simplify_free_variable env arg ~f:(fun env arg arg_approx ->
      match A.check_approx_for_string arg_approx with
      | None ->
        let env = E.inside_branch env in
        let sw, r =
          List.fold_right (fun (str, lam) (sw, r) ->
              let approx = R.approx r in
              let lam, r = simplify env r lam in
              (str, lam)::sw,
                R.meet_approx r env approx)
            sw
            ([], r)
        in
        let def, r =
          match def with
          | None -> def, r
          | Some def ->
            let approx = R.approx r in
            let def, r = simplify env r def in
            Some def,
              R.meet_approx r env approx
        in
        String_switch (arg, sw, def), ret r (A.value_unknown Other)
      | Some arg_string ->
        let branch =
          match List.find (fun (str, _) -> str = arg_string) sw with
          | (_, branch) -> branch
          | exception Not_found ->
            match def with
            | None ->
              Flambda.Proved_unreachable
            | Some def ->
              def
        in
        let branch, r = simplify env r branch in
        branch, R.map_benefit r B.remove_branch)
  | Proved_unreachable -> tree, ret r A.value_bottom

and simplify_list env r l =
  match l with
  | [] -> [], [], r
  | h::t ->
    let t', approxs, r = simplify_list env r t in
    let h', r = simplify env r h in
    let approxs = (R.approx r) :: approxs in
    if t' == t && h' == h
    then l, approxs, r
    else h' :: t', approxs, r

and duplicate_function ~env ~(set_of_closures : Flambda.set_of_closures)
      ~fun_var =
  let function_decl =
    match Variable.Map.find fun_var set_of_closures.function_decls.funs with
    | exception Not_found ->
      Misc.fatal_errorf "duplicate_function: cannot find function %a"
        Variable.print fun_var
    | function_decl -> function_decl
  in
  let env = E.activate_freshening (E.set_never_inline env) in
  let free_vars, specialised_args, function_decls, parameter_approximations,
      _internal_value_set_of_closures, set_of_closures_env =
    Inline_and_simplify_aux.prepare_to_simplify_set_of_closures ~env
      ~set_of_closures ~function_decls:set_of_closures.function_decls
      ~freshen:false ~only_for_function_decl:(Some function_decl)
  in
  let function_decl =
    match Variable.Map.find fun_var function_decls.funs with
    | exception Not_found ->
      Misc.fatal_errorf "duplicate_function: cannot find function %a (2)"
        Variable.print fun_var
    | function_decl -> function_decl
  in
  let closure_env =
    Inline_and_simplify_aux.prepare_to_simplify_closure ~function_decl
      ~free_vars ~specialised_args ~parameter_approximations
      ~set_of_closures_env
  in
  let body, _r =
    E.enter_closure closure_env
      ~closure_id:(Closure_id.wrap fun_var)
      ~inline_inside:false
      ~dbg:function_decl.dbg
      ~f:(fun body_env ->
        simplify body_env (R.create ()) function_decl.body)
  in
  let function_decl =
    Flambda.create_function_declaration ~params:function_decl.params
      ~body ~stub:function_decl.stub ~dbg:function_decl.dbg
      ~inline:function_decl.inline ~specialise:function_decl.specialise
      ~is_a_functor:function_decl.is_a_functor
  in
  function_decl, specialised_args

let constant_defining_value_approx
    env
    (constant_defining_value:Flambda.constant_defining_value) =
  match constant_defining_value with
  | Allocated_const const ->
    approx_for_allocated_const const
  | Block (tag, fields) ->
    let fields =
      List.map
        (function
          | Flambda.Symbol sym -> begin
              match E.find_symbol_opt env sym with
              | Some approx -> approx
              | None -> A.value_unresolved (Symbol sym)
            end
          | Flambda.Const cst -> simplify_const cst)
        fields
    in
    A.value_block tag (Array.of_list fields)
  | Set_of_closures { function_decls; free_vars; specialised_args } ->
    (* At toplevel, there is no freshening currently happening (this
       cannot be the body of a currently inlined function), so we can
       keep the original set_of_closures in the approximation. *)
    assert(E.freshening env = Freshening.empty);
    assert(Variable.Map.is_empty free_vars);
    assert(Variable.Map.is_empty specialised_args);
    let invariant_params =
      lazy (Invariant_params.invariant_params_in_recursion function_decls
        ~backend:(E.backend env))
    in
    let value_set_of_closures =
      A.create_value_set_of_closures ~function_decls
        ~bound_vars:Var_within_closure.Map.empty
        ~invariant_params
        ~specialised_args:Variable.Map.empty
        ~freshening:Freshening.Project_var.empty
        ~direct_call_surrogates:Closure_id.Map.empty
    in
    A.value_set_of_closures value_set_of_closures
  | Project_closure (set_of_closures_symbol, closure_id) -> begin
      match E.find_symbol_opt env set_of_closures_symbol with
      | None ->
        A.value_unresolved (Symbol set_of_closures_symbol)
      | Some set_of_closures_approx ->
        let checked_approx =
          A.check_approx_for_set_of_closures set_of_closures_approx
        in
        match checked_approx with
        | Ok (_, value_set_of_closures) ->
          let closure_id =
            A.freshen_and_check_closure_id value_set_of_closures closure_id
          in
          A.value_closure value_set_of_closures closure_id
        | Unresolved sym -> A.value_unresolved sym
        | Unknown -> A.value_unknown Other
        | Unknown_because_of_unresolved_value value ->
          A.value_unknown (Unresolved_value value)
        | Wrong ->
          Misc.fatal_errorf "Wrong approximation for [Project_closure] \
                             when being used as a [constant_defining_value]: %a"
            Flambda.print_constant_defining_value constant_defining_value
    end

(* See documentation on [Let_rec_symbol] in flambda.mli. *)
let define_let_rec_symbol_approx env defs =
  (* First declare an empty version of the symbols *)
  let env =
    List.fold_left (fun env (symbol, _) ->
        E.add_symbol env symbol (A.value_unresolved (Symbol symbol)))
      env defs
  in
  let rec loop times env =
    if times <= 0 then
      env
    else
      let env =
        List.fold_left (fun newenv (symbol, constant_defining_value) ->
            let approx =
              constant_defining_value_approx env constant_defining_value
            in
            let approx = A.augment_with_symbol approx symbol in
            E.redefine_symbol newenv symbol approx)
          env defs
      in
      loop (times-1) env
  in
  loop 2 env

let simplify_constant_defining_value
    env r symbol
    (constant_defining_value:Flambda.constant_defining_value) =
  let r, constant_defining_value, approx =
    match constant_defining_value with
    (* No simplifications are possible for [Allocated_const] or [Block]. *)
    | Allocated_const const ->
      r, constant_defining_value, approx_for_allocated_const const
    | Block (tag, fields) ->
      let fields = List.map
          (function
            | Flambda.Symbol sym -> E.find_symbol_exn env sym
            | Flambda.Const cst -> simplify_const cst)
          fields
      in
      r, constant_defining_value, A.value_block tag (Array.of_list fields)
    | Set_of_closures set_of_closures ->
      if Variable.Map.cardinal set_of_closures.free_vars <> 0 then begin
        Misc.fatal_errorf "Set of closures bound by [Let_symbol] is not \
                           closed: %a"
          Flambda.print_set_of_closures set_of_closures
      end;
      let set_of_closures, r, _freshening =
        simplify_set_of_closures env r set_of_closures
      in
      r, ((Set_of_closures set_of_closures) : Flambda.constant_defining_value),
        R.approx r
    | Project_closure (set_of_closures_symbol, closure_id) ->
      (* No simplifications are necessary here. *)
      let set_of_closures_approx =
        E.find_symbol_exn env set_of_closures_symbol
      in
      let closure_approx =
        match A.check_approx_for_set_of_closures set_of_closures_approx with
        | Ok (_, value_set_of_closures) ->
          let closure_id =
            A.freshen_and_check_closure_id value_set_of_closures closure_id
          in
          A.value_closure value_set_of_closures closure_id
        | Unresolved sym -> A.value_unresolved sym
        | Unknown -> A.value_unknown Other
        | Unknown_because_of_unresolved_value value ->
          A.value_unknown (Unresolved_value value)
        | Wrong ->
          Misc.fatal_errorf "Wrong approximation for [Project_closure] \
                             when being used as a [constant_defining_value]: %a"
            Flambda.print_constant_defining_value constant_defining_value
      in
      r, constant_defining_value, closure_approx
  in
  let approx = A.augment_with_symbol approx symbol in
  let r = ret r approx in
  r, constant_defining_value, approx

let rec simplify_program_body env r (program : Flambda.program_body)
  : Flambda.program_body * R.t =
  match program with
  | Let_rec_symbol (defs, program) ->
    let env = define_let_rec_symbol_approx env defs in
    let env, r, defs =
      List.fold_left (fun (newenv, r, defs) (symbol, def) ->
          let r, def, approx =
            simplify_constant_defining_value env r symbol def
          in
          let approx = A.augment_with_symbol approx symbol in
          let newenv = E.redefine_symbol newenv symbol approx in
          (newenv, r, (symbol, def) :: defs))
        (env, r, []) defs
    in
    let program, r = simplify_program_body env r program in
    Let_rec_symbol (defs, program), r
  | Let_symbol (symbol, constant_defining_value, program) ->
    let r, constant_defining_value, approx =
      simplify_constant_defining_value env r symbol constant_defining_value
    in
    let approx = A.augment_with_symbol approx symbol in
    let env = E.add_symbol env symbol approx in
    let program, r = simplify_program_body env r program in
    Let_symbol (symbol, constant_defining_value, program), r
  | Initialize_symbol (symbol, tag, fields, program) ->
    let fields, approxs, r = simplify_list env r fields in
    let approx =
      A.augment_with_symbol (A.value_block tag (Array.of_list approxs)) symbol
    in
    let module Backend = (val (E.backend env) : Backend_intf.S) in
    let env = E.add_symbol env symbol approx in
    let program, r = simplify_program_body env r program in
    Initialize_symbol (symbol, tag, fields, program), r
  | Effect (expr, program) ->
    let expr, r = simplify env r expr in
    let program, r = simplify_program_body env r program in
    Effect (expr, program), r
  | End root -> End root, r

let simplify_program env r (program : Flambda.program) =
  let env, r =
    Symbol.Set.fold (fun symbol (env, r) ->
        let env, approx =
          match E.find_symbol_exn env symbol with
          | exception Not_found ->
            let module Backend = (val (E.backend env) : Backend_intf.S) in
            (* CR-someday mshinwell for mshinwell: Is there a reason we cannot
               use [simplify_named_using_approx_and_env] here? *)
            let approx = Backend.import_symbol symbol in
            E.add_symbol env symbol approx, approx
          | approx -> env, approx
        in
        env, ret r approx)
      program.imported_symbols
      (env, r)
  in
  let program_body, r = simplify_program_body env r program.program_body in
  let program = { program with program_body; } in
  program, r

let add_predef_exns_to_environment ~env ~backend =
  let module Backend = (val backend : Backend_intf.S) in
  List.fold_left (fun env predef_exn ->
      assert (Ident.is_predef_exn predef_exn);
      let symbol = Backend.symbol_for_global' predef_exn in
      let name = Ident.name predef_exn in
      let approx =
        A.value_block Tag.object_tag
          [| A.value_string (String.length name) (Some name);
             A.value_unknown Other;
          |]
      in
      E.add_symbol env symbol (A.augment_with_symbol approx symbol))
    env
    Predef.all_predef_exns

let run ~never_inline ~backend ~prefixname ~round program =
  let r = R.create () in
  let report = !Clflags.inlining_report in
  if never_inline then Clflags.inlining_report := false;
  let initial_env =
    add_predef_exns_to_environment
      ~env:(E.create ~never_inline ~backend ~round)
      ~backend
  in
  let result, r = simplify_program initial_env r program in
  let result = Flambda_utils.introduce_needed_import_symbols result in
  if not (Static_exception.Set.is_empty (R.used_static_exceptions r))
  then begin
    Misc.fatal_error (Format.asprintf "Remaining static exceptions: %a@.%a@."
      Static_exception.Set.print (R.used_static_exceptions r)
      Flambda.print_program result)
  end;
  assert (Static_exception.Set.is_empty (R.used_static_exceptions r));
  if !Clflags.inlining_report then begin
    let output_prefix = Printf.sprintf "%s.%d" prefixname round in
    Inlining_stats.save_then_forget_decisions ~output_prefix
  end;
  Clflags.inlining_report := report;
  result
