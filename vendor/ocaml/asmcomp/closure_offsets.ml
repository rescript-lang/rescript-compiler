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

type result = {
  function_offsets : int Closure_id.Map.t;
  free_variable_offsets : int Var_within_closure.Map.t;
}

let add_closure_offsets
      { function_offsets; free_variable_offsets }
      ({ function_decls; free_vars } : Flambda.set_of_closures) =
  (* Build the table mapping the functions declared by the set of closures
     to the positions of their individual "infix" closures inside the runtime
     closure block.  (All of the environment entries will come afterwards.) *)
  let assign_function_offset id function_decl (map, env_pos) =
    let pos = env_pos + 1 in
    let env_pos =
      let arity = Flambda_utils.function_arity function_decl in
      env_pos
        + 1  (* GC header; either [Closure_tag] or [Infix_tag] *)
        + 1  (* full application code pointer *)
        + 1  (* arity *)
        + (if arity > 1 then 1 else 0)  (* partial application code pointer *)
    in
    let closure_id = Closure_id.wrap id in
    if Closure_id.Map.mem closure_id map then begin
      Misc.fatal_errorf "Closure_offsets.add_closure_offsets: function \
          offset for %a would be defined multiple times"
        Closure_id.print closure_id
    end;
    let map = Closure_id.Map.add closure_id pos map in
    (map, env_pos)
  in
  let function_offsets, free_variable_pos =
    Variable.Map.fold assign_function_offset
      function_decls.funs (function_offsets, -1)
  in
  (* Adds the mapping of free variables to their offset.  Recall that
     projections of [Var_within_closure]s are only currently used when
     compiling accesses to the closure of a function from outside that
     function (in particular, as a result of inlining).  Accesses to
     a function's own closure are compiled directly via normal [Var]
     accesses. *)
  (* CR-someday mshinwell: As discussed with lwhite, maybe this isn't
     ideal, and the self accesses should be explicitly marked too. *)
  let assign_free_variable_offset var _ (map, pos) =
    let var_within_closure = Var_within_closure.wrap var in
    if Var_within_closure.Map.mem var_within_closure map then begin
      Misc.fatal_errorf "Closure_offsets.add_closure_offsets: free variable \
          offset for %a would be defined multiple times"
        Var_within_closure.print var_within_closure
    end;
    let map = Var_within_closure.Map.add var_within_closure pos map in
    (map, pos + 1)
  in
  let free_variable_offsets, _ =
    Variable.Map.fold assign_free_variable_offset
      free_vars (free_variable_offsets, free_variable_pos)
  in
  { function_offsets;
    free_variable_offsets;
  }

let compute (program:Flambda.program) =
  let init : result =
    { function_offsets = Closure_id.Map.empty;
      free_variable_offsets = Var_within_closure.Map.empty;
    }
  in
  let r =
    List.fold_left add_closure_offsets
      init (Flambda_utils.all_sets_of_closures program)
  in
  r

let compute_reexported_offsets program
      ~current_unit_offset_fun ~current_unit_offset_fv
      ~imported_units_offset_fun ~imported_units_offset_fv =
  let offset_fun = ref current_unit_offset_fun in
  let offset_fv = ref current_unit_offset_fv in
  let used_closure_id closure_id =
    match Closure_id.Map.find closure_id imported_units_offset_fun with
    | offset ->
      assert (not (Closure_id.Map.mem closure_id current_unit_offset_fun));
      begin match Closure_id.Map.find closure_id !offset_fun with
      | exception Not_found ->
        offset_fun := Closure_id.Map.add closure_id offset !offset_fun
      | offset' -> assert (offset = offset')
      end
    | exception Not_found ->
      assert (Closure_id.Map.mem closure_id current_unit_offset_fun)
  in
  let used_var_within_closure var =
    match Var_within_closure.Map.find var imported_units_offset_fv with
    | offset ->
      assert (not (Var_within_closure.Map.mem var current_unit_offset_fv));
      begin match Var_within_closure.Map.find var !offset_fv with
      | exception Not_found ->
        offset_fv := Var_within_closure.Map.add var offset !offset_fv
      | offset' -> assert (offset = offset')
      end
    | exception Not_found ->
      assert (Var_within_closure.Map.mem var current_unit_offset_fv)
  in
  Flambda_iterators.iter_named_of_program program
    ~f:(fun (named : Flambda.named) ->
      match named with
      | Project_closure { closure_id; _ } ->
        used_closure_id closure_id
      | Move_within_set_of_closures { start_from; move_to; _ } ->
        used_closure_id start_from;
        used_closure_id move_to
      | Project_var { closure_id; var; _ } ->
        used_closure_id closure_id;
        used_var_within_closure var
      | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Read_symbol_field _ | Set_of_closures _ | Prim _ | Expr _ -> ());
  Flambda_iterators.iter_constant_defining_values_on_program program
    ~f:(fun (const : Flambda.constant_defining_value) ->
      match const with
      | Project_closure (_, closure_id) -> used_closure_id closure_id
      | Allocated_const _ | Block _ | Set_of_closures _ -> ());
  !offset_fun, !offset_fv
