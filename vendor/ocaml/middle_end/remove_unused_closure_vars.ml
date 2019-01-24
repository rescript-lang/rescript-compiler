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

(** A variable in a closure can either be used by the closure itself
    or by an inlined version of the function. *)
let remove_unused_closure_variables ~remove_direct_call_surrogates program =
  let used_vars_within_closure, used_closure_ids =
    let used = Var_within_closure.Tbl.create 13 in
    let used_fun = Closure_id.Tbl.create 13 in
    let aux_named (named : Flambda.named) =
      match named with
      | Project_closure { set_of_closures = _; closure_id } ->
        Closure_id.Tbl.add used_fun closure_id ()
      | Project_var { closure_id; var } ->
        Var_within_closure.Tbl.add used var ();
        Closure_id.Tbl.add used_fun closure_id ()
      | Move_within_set_of_closures { closure = _; start_from; move_to } ->
        Closure_id.Tbl.add used_fun start_from ();
        Closure_id.Tbl.add used_fun move_to ()
      | Symbol _ | Const _ | Set_of_closures _ | Prim _ | Expr _
      | Allocated_const _ | Read_mutable _ | Read_symbol_field _ -> ()
    in
    Flambda_iterators.iter_named_of_program ~f:aux_named program;
    used, used_fun
  in
  let aux_named _ (named : Flambda.named) : Flambda.named =
    match named with
    | Set_of_closures ({ function_decls; free_vars; _ } as set_of_closures) ->
      let direct_call_surrogates =
        if remove_direct_call_surrogates then Variable.Set.empty
        else
          Variable.Set.of_list
            (Variable.Map.data set_of_closures.direct_call_surrogates)
      in
      let rec add_needed needed_funs remaining_funs free_vars_of_kept_funs =
        let new_needed_funs, remaining_funs =
          (* Keep a function if it is used either by the rest of the code,
             (in used_closure_ids), or by any other kept function
             (in free_vars_of_kept_funs) *)
          Variable.Map.partition (fun fun_id _ ->
              Variable.Set.mem fun_id free_vars_of_kept_funs
              || Closure_id.Tbl.mem used_closure_ids
                (Closure_id.wrap fun_id)
              || Variable.Set.mem fun_id direct_call_surrogates)
            remaining_funs
        in
        if Variable.Map.is_empty new_needed_funs then
          (* If no new function is needed, we reached fixpoint *)
          needed_funs, free_vars_of_kept_funs
        else begin
          let needed_funs =
            Variable.Map.disjoint_union needed_funs new_needed_funs
          in
          let free_vars_of_kept_funs =
            Variable.Map.fold (fun _ { Flambda. free_variables } acc ->
                Variable.Set.union free_variables acc)
              new_needed_funs
              free_vars_of_kept_funs
          in
          add_needed needed_funs remaining_funs free_vars_of_kept_funs
        end
      in
      let funs, free_vars_of_kept_funs =
        add_needed Variable.Map.empty function_decls.funs Variable.Set.empty
      in
      let free_vars =
        Variable.Map.filter (fun id _var ->
            Variable.Set.mem id free_vars_of_kept_funs
            || Var_within_closure.Tbl.mem
                 used_vars_within_closure
                 (Var_within_closure.wrap id))
          free_vars
      in
      let function_decls =
        Flambda.update_function_declarations function_decls ~funs
      in
      let specialised_args =
        (* Remove specialised args that are used by removed functions *)
        let all_remaining_arguments =
          Variable.Map.fold (fun _ { Flambda.params } set ->
              Variable.Set.union set (Parameter.Set.vars params))
            funs Variable.Set.empty
        in
        Variable.Map.filter (fun arg _ ->
            Variable.Set.mem arg all_remaining_arguments)
          set_of_closures.specialised_args
      in
      let free_vars =
        Flambda_utils.clean_projections ~which_variables:free_vars
      in
      let direct_call_surrogates =
        (* Remove direct call surrogates where either the existing function
           or the surrogate has been eliminated. *)
        Variable.Map.fold (fun existing surrogate surrogates ->
            if not (Variable.Map.mem existing funs)
              || not (Variable.Map.mem surrogate funs)
            then surrogates
            else Variable.Map.add existing surrogate surrogates)
          set_of_closures.direct_call_surrogates
          Variable.Map.empty
      in
      let set_of_closures =
        Flambda.create_set_of_closures ~function_decls
          ~free_vars ~specialised_args ~direct_call_surrogates
      in
      Set_of_closures set_of_closures
    | e -> e
  in
  Flambda_iterators.map_named_of_program ~f:aux_named program
