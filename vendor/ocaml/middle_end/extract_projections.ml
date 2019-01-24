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
module E = Inline_and_simplify_aux.Env

(* CR-soon pchambart: should we restrict only to cases
  when the field is aliased to a variable outside
  of the closure (i.e. when we can certainly remove
  the allocation of the block) ?
  Note that this may prevent cases with imbricated
  closures from benefiting from this transformations.
  mshinwell: What word was "imbricated" supposed to be?
  (The code this referred to has been deleted, but the same thing is
  probably still happening).
*)

let known_valid_projections ~env ~projections ~which_variables =
  Projection.Set.filter (fun projection ->
      let from = Projection.projecting_from projection in
      let outer_var =
        match Variable.Map.find from which_variables with
        | exception Not_found -> assert false
        | (outer_var : Flambda.specialised_to) ->
          Freshening.apply_variable (E.freshening env) outer_var.var
      in
      let approx = E.find_exn env outer_var in
      match projection with
      | Project_var project_var ->
        begin match A.check_approx_for_closure approx with
        | Ok (_value_closure, _approx_var, _approx_sym,
              value_set_of_closures) ->
          Var_within_closure.Map.mem project_var.var
            value_set_of_closures.bound_vars
        | Wrong -> false
        end
      | Project_closure project_closure ->
        begin match A.strict_check_approx_for_set_of_closures approx with
        | Ok (_var, value_set_of_closures) ->
          Variable.Set.mem (Closure_id.unwrap project_closure.closure_id)
            (Variable.Map.keys value_set_of_closures.function_decls.funs)
        | Wrong -> false
        end
      | Move_within_set_of_closures move ->
        begin match A.check_approx_for_closure approx with
        | Ok (value_closure, _approx_var, _approx_sym,
              _value_set_of_closures) ->
          (* We could check that [move.move_to] is in [value_set_of_closures],
             but this is unnecessary, since [Closure_id]s are unique. *)
          Closure_id.equal value_closure.closure_id move.start_from
        | Wrong -> false
        end
      | Field (field_index, _) ->
        match A.check_approx_for_block approx with
        | Wrong -> false
        | Ok (_tag, fields) ->
          field_index >= 0 && field_index < Array.length fields)
    projections

let rec analyse_expr ~which_variables expr =
  let projections = ref Projection.Set.empty in
  let used_which_variables = ref Variable.Set.empty in
  let check_free_variable var =
    if Variable.Map.mem var which_variables then begin
      used_which_variables := Variable.Set.add var !used_which_variables
    end
  in
  let for_expr (expr : Flambda.expr) =
    match expr with
    | Var var
    | Let_mutable { initial_value = var } ->
      check_free_variable var
    (* CR-soon mshinwell: We don't handle [Apply] for the moment to
       avoid disabling unboxing optimizations whenever we see a recursive
       call.  We should improve this analysis.  Leo says this can be
       done by a similar thing to the unused argument analysis. *)
    | Apply _ -> ()
    | Send { meth; obj; args; _ } ->
      check_free_variable meth;
      check_free_variable obj;
      List.iter check_free_variable args
    | Assign { new_value; _ } ->
      check_free_variable new_value
    | If_then_else (var, _, _)
    | Switch (var, _)
    | String_switch (var, _, _) ->
      check_free_variable var
    | Static_raise (_, args) ->
      List.iter check_free_variable args
    | For { from_value; to_value; _ } ->
      check_free_variable from_value;
      check_free_variable to_value
    | Let _ | Let_rec _ | Static_catch _ | While _ | Try_with _
    | Proved_unreachable -> ()
  in
  let for_named (named : Flambda.named) =
    match named with
    | Project_var project_var
        when Variable.Map.mem project_var.closure which_variables ->
      projections :=
        Projection.Set.add (Project_var project_var) !projections
    | Project_closure project_closure
        when Variable.Map.mem project_closure.set_of_closures
          which_variables ->
      projections :=
        Projection.Set.add (Project_closure project_closure) !projections
    | Move_within_set_of_closures move
        when Variable.Map.mem move.closure which_variables ->
      projections :=
        Projection.Set.add (Move_within_set_of_closures move) !projections
    | Prim (Pfield (field_index, _), [var], _dbg)
        when Variable.Map.mem var which_variables ->
      projections :=
        Projection.Set.add (Field (field_index, var)) !projections
    | Set_of_closures set_of_closures ->
      let aliasing_free_vars =
        Variable.Map.filter (fun _ (spec_to : Flambda.specialised_to) ->
            Variable.Map.mem spec_to.var which_variables)
          set_of_closures.free_vars
      in
      let aliasing_specialised_args =
        Variable.Map.filter (fun _ (spec_to : Flambda.specialised_to) ->
            Variable.Map.mem spec_to.var which_variables)
          set_of_closures.specialised_args
      in
      let aliasing_vars =
        Variable.Map.disjoint_union
          aliasing_free_vars aliasing_specialised_args
      in
      if not (Variable.Map.is_empty aliasing_vars) then begin
        Variable.Map.iter (fun _ (fun_decl : Flambda.function_declaration) ->
          (* We ignore projections from within nested sets of closures. *)
          let _, used =
            analyse_expr fun_decl.body ~which_variables:aliasing_vars
          in
          Variable.Set.iter (fun var ->
            match Variable.Map.find var aliasing_vars with
            | exception Not_found -> assert false
            | spec_to -> check_free_variable spec_to.var)
            used)
          set_of_closures.function_decls.funs
      end
    | Prim (_, vars, _) ->
      List.iter check_free_variable vars
    | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
    | Read_symbol_field _ | Project_var _ | Project_closure _
    | Move_within_set_of_closures _
    | Expr _ -> ()
  in
  Flambda_iterators.iter_toplevel for_expr for_named expr;
  let projections = !projections in
  let used_which_variables = !used_which_variables in
  projections, used_which_variables

let from_function_decl ~env ~which_variables
      ~(function_decl : Flambda.function_declaration) =
  let projections, used_which_variables =
    analyse_expr ~which_variables function_decl.body
  in
  (* We must use approximation information to determine which projections
     are actually valid in the current environment, other we might lift
     expressions too far. *)
  let projections =
    known_valid_projections ~env ~projections ~which_variables
  in
  (* Don't extract projections whose [projecting_from] variable is also
     used boxed.  We could in the future consider being more sophisticated
     about this based on the uses in the body, but given we are not doing
     that yet, it seems safest in performance terms not to (e.g.) unbox a
     specialised argument whose boxed version is used. *)
  Projection.Set.filter (fun projection ->
      let projecting_from = Projection.projecting_from projection in
      not (Variable.Set.mem projecting_from used_which_variables))
    projections
