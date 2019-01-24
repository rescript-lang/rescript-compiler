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

module B = Inlining_cost.Benefit

let pass_name = "unbox-free-vars-of-closures"
let () = Pass_wrapper.register ~pass_name
let variable_suffix = ""

(* CR-someday mshinwell: Nearly but not quite the same as something that
   Augment_specialised_args uses. *)
let add_lifted_projections_around_set_of_closures
      ~set_of_closures ~existing_inner_to_outer_vars ~benefit
      ~definitions_indexed_by_new_inner_vars =
  let body =
    Flambda_utils.name_expr (Set_of_closures set_of_closures)
      ~name:pass_name
  in
  Variable.Map.fold (fun new_inner_var (projection : Projection.t)
            (expr, benefit) ->
      let find_outer_var inner_var =
        match
          Variable.Map.find inner_var existing_inner_to_outer_vars
        with
        | (outer_var : Flambda.specialised_to) -> outer_var.var
        | exception Not_found ->
          Misc.fatal_errorf "(UFV) find_outer_var: expected %a \
              to be in [existing_inner_to_outer_vars], but it is \
              not.  (The projection was: %a)"
            Variable.print inner_var
            Projection.print projection
      in
      let benefit = B.add_projection projection benefit in
      let named : Flambda.named =
        (* The lifted projection must be in terms of outer variables,
           not inner variables. *)
        let projection =
          Projection.map_projecting_from projection ~f:find_outer_var
        in
        Flambda_utils.projection_to_named projection
      in
      let expr =
        Flambda.create_let (find_outer_var new_inner_var) named expr
      in
      (expr, benefit))
    definitions_indexed_by_new_inner_vars
    (body, benefit)

let run ~env ~(set_of_closures : Flambda.set_of_closures) =
  if not !Clflags.unbox_free_vars_of_closures then
    None
  else
    let definitions_indexed_by_new_inner_vars, _, free_vars, done_something =
      let all_existing_definitions =
        Variable.Map.fold (fun _inner_var (outer_var : Flambda.specialised_to)
              all_existing_definitions ->
            match outer_var.projection with
            | None -> all_existing_definitions
            | Some projection ->
              Projection.Set.add projection all_existing_definitions)
          set_of_closures.free_vars
          Projection.Set.empty
      in
      Flambda_iterators.fold_function_decls_ignoring_stubs set_of_closures
        ~init:(Variable.Map.empty, all_existing_definitions,
          set_of_closures.free_vars, false)
        ~f:(fun ~fun_var:_ ~function_decl result ->
          let extracted =
            Extract_projections.from_function_decl ~env ~function_decl
              ~which_variables:set_of_closures.free_vars
          in
          Projection.Set.fold (fun projection
                ((definitions_indexed_by_new_inner_vars,
                  all_existing_definitions_including_added_ones,
                  additional_free_vars, _done_something) as result) ->
              (* Don't add a new free variable if there already exists a
                 free variable with the desired projection.  We need to
                 dedup not only across the existing free variables but
                 also across newly-added ones (unlike in
                 [Augment_specialised_args]), since free variables are
                 not local to a function declaration but rather to a
                 set of closures. *)
              if Projection.Set.mem projection
                all_existing_definitions_including_added_ones
              then begin
                result
              end else begin
                (* Add a new free variable.  This needs both a fresh
                   "new inner" and a fresh "new outer" var, since we know
                   the definition is not a duplicate. *)
                let projecting_from = Projection.projecting_from projection in
                let new_inner_var =
                  Variable.rename projecting_from
                    ~append:variable_suffix
                in
                let new_outer_var =
                  Variable.rename projecting_from
                    ~append:variable_suffix
                in
                let definitions_indexed_by_new_inner_vars =
                  Variable.Map.add new_inner_var projection
                    definitions_indexed_by_new_inner_vars
                in
                let all_existing_definitions_including_added_ones =
                  Projection.Set.add projection
                    all_existing_definitions_including_added_ones
                in
                let new_outer_var : Flambda.specialised_to =
                  { var = new_outer_var;
                    projection = Some projection;
                  }
                in
                let additional_free_vars =
                  Variable.Map.add new_inner_var new_outer_var
                    additional_free_vars
                in
                definitions_indexed_by_new_inner_vars,
                  all_existing_definitions_including_added_ones,
                  additional_free_vars,
                  true
              end)
            extracted
            result)
    in
    if not done_something then
      None
    else
      (* CR-someday mshinwell: could consider doing the grouping thing
         similar to Augment_specialised_args *)
      let num_free_vars_before =
        Variable.Map.cardinal set_of_closures.free_vars
      in
      let num_free_vars_after =
        Variable.Map.cardinal free_vars
      in
      assert (num_free_vars_after > num_free_vars_before);
      (* Don't let the closure grow too large. *)
      if num_free_vars_after > 2 * num_free_vars_before then
        None
      else
        let set_of_closures =
          Flambda.create_set_of_closures
            ~function_decls:set_of_closures.function_decls
            ~free_vars
            ~specialised_args:set_of_closures.specialised_args
            ~direct_call_surrogates:set_of_closures.direct_call_surrogates
        in
        let expr, benefit =
          add_lifted_projections_around_set_of_closures ~set_of_closures
            ~benefit:B.zero
            ~existing_inner_to_outer_vars:set_of_closures.free_vars
            ~definitions_indexed_by_new_inner_vars
        in
        Some (expr, benefit)

let run ~env ~set_of_closures =
  Pass_wrapper.with_dump ~pass_name ~input:set_of_closures
    ~print_input:Flambda.print_set_of_closures
    ~print_output:(fun ppf (expr, _) -> Flambda.print ppf expr)
    ~f:(fun () -> run ~env ~set_of_closures)
