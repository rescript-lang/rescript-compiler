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

let rename_id_state = Export_id.Tbl.create 100
let rename_set_of_closures_id_state = Set_of_closures_id.Tbl.create 10
let imported_function_declarations_table =
  (Set_of_closures_id.Tbl.create 10
   : Flambda.function_declarations Set_of_closures_id.Tbl.t)

(* Rename export identifiers' compilation units to denote that they now
   live within a pack. *)
let import_eid_for_pack units pack id =
  try Export_id.Tbl.find rename_id_state id
  with Not_found ->
    let unit_id = Export_id.get_compilation_unit id in
    let id' =
      if Compilation_unit.Set.mem unit_id units
      then Export_id.create ?name:(Export_id.name id) pack
      else id
    in
    Export_id.Tbl.add rename_id_state id id';
    id'

(* Similar to [import_eid_for_pack], but for symbols. *)
let import_symbol_for_pack units pack symbol =
  let compilation_unit = Symbol.compilation_unit symbol in
  if Compilation_unit.Set.mem compilation_unit units
  then Symbol.import_for_pack ~pack symbol
  else symbol

let import_approx_for_pack units pack (approx : Export_info.approx)
      : Export_info.approx =
  match approx with
  | Value_symbol sym -> Value_symbol (import_symbol_for_pack units pack sym)
  | Value_id eid -> Value_id (import_eid_for_pack units pack eid)
  | Value_unknown -> Value_unknown

let import_set_of_closures_id_for_pack units pack
    (set_of_closures_id : Set_of_closures_id.t)
      : Set_of_closures_id.t =
  let compilation_unit =
    Set_of_closures_id.get_compilation_unit set_of_closures_id
  in
  if Compilation_unit.Set.mem compilation_unit units then
    Set_of_closures_id.Tbl.memoize
      rename_set_of_closures_id_state
      (fun _ ->
         Set_of_closures_id.create
           ?name:(Set_of_closures_id.name set_of_closures_id)
           pack)
      set_of_closures_id
  else set_of_closures_id

let import_set_of_closures_origin_for_pack units pack
    (set_of_closures_origin : Set_of_closures_origin.t)
    : Set_of_closures_origin.t =
  Set_of_closures_origin.rename
    (import_set_of_closures_id_for_pack units pack)
    set_of_closures_origin

let import_set_of_closures units pack
      (set_of_closures : Export_info.value_set_of_closures)
      : Export_info.value_set_of_closures =
  { set_of_closures_id =
      import_set_of_closures_id_for_pack units pack
        set_of_closures.set_of_closures_id;
    bound_vars =
      Var_within_closure.Map.map (import_approx_for_pack units pack)
        set_of_closures.bound_vars;
    results =
      Closure_id.Map.map (import_approx_for_pack units pack)
        set_of_closures.results;
    aliased_symbol =
      Misc.may_map
        (import_symbol_for_pack units pack)
        set_of_closures.aliased_symbol;
  }

let import_descr_for_pack units pack (descr : Export_info.descr)
      : Export_info.descr =
  match descr with
  | Value_int _
  | Value_char _
  | Value_constptr _
  | Value_string _
  | Value_float _
  | Value_float_array _
  | Export_info.Value_boxed_int _
  | Value_mutable_block _ as desc -> desc
  | Value_block (tag, fields) ->
    Value_block (tag, Array.map (import_approx_for_pack units pack) fields)
  | Value_closure { closure_id; set_of_closures } ->
    Value_closure {
      closure_id;
      set_of_closures = import_set_of_closures units pack set_of_closures;
    }
  | Value_set_of_closures set_of_closures ->
    Value_set_of_closures (import_set_of_closures units pack set_of_closures)

let rec import_code_for_pack units pack expr =
  Flambda_iterators.map_named (function
      | Symbol sym -> Symbol (import_symbol_for_pack units pack sym)
      | Read_symbol_field (sym, field) ->
        Read_symbol_field (import_symbol_for_pack units pack sym, field)
      | Set_of_closures set_of_closures ->
        let set_of_closures =
          Flambda.create_set_of_closures
            ~free_vars:set_of_closures.free_vars
            ~specialised_args:set_of_closures.specialised_args
            ~direct_call_surrogates:set_of_closures.direct_call_surrogates
            ~function_decls:
              (import_function_declarations_for_pack units pack
                 set_of_closures.function_decls)
        in
        Set_of_closures set_of_closures
      | e -> e)
    expr

and import_function_declarations_for_pack_aux units pack
      (function_decls : Flambda.function_declarations) =
  let funs =
    Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
        Flambda.create_function_declaration ~params:function_decl.params
          ~body:(import_code_for_pack units pack function_decl.body)
          ~stub:function_decl.stub ~dbg:function_decl.dbg
          ~inline:function_decl.inline
          ~specialise:function_decl.specialise
          ~is_a_functor:function_decl.is_a_functor)
      function_decls.funs
  in
  Flambda.import_function_declarations_for_pack
    (Flambda.update_function_declarations function_decls ~funs)
    (import_set_of_closures_id_for_pack units pack)
    (import_set_of_closures_origin_for_pack units pack)

and import_function_declarations_for_pack units pack
    (function_decls:Flambda.function_declarations) =
  let original_set_of_closures_id = function_decls.set_of_closures_id in
  try
    Set_of_closures_id.Tbl.find imported_function_declarations_table
      original_set_of_closures_id
  with Not_found ->
    let function_decls =
      import_function_declarations_for_pack_aux units pack function_decls
    in
    Set_of_closures_id.Tbl.add
      imported_function_declarations_table
      original_set_of_closures_id
      function_decls;
    function_decls

let import_eidmap_for_pack units pack f map =
  Export_info.nest_eid_map
    (Compilation_unit.Map.fold
      (fun _ map acc -> Export_id.Map.disjoint_union map acc)
      (Compilation_unit.Map.map (fun map ->
          Export_id.Map.map_keys (import_eid_for_pack units pack)
            (Export_id.Map.map f map))
        map)
      Export_id.Map.empty)

let import_for_pack ~pack_units ~pack (exp : Export_info.t) =
  let import_sym = import_symbol_for_pack pack_units pack in
  let import_descr = import_descr_for_pack pack_units pack in
  let import_eid = import_eid_for_pack pack_units pack in
  let import_eidmap f map = import_eidmap_for_pack pack_units pack f map in
  let import_set_of_closures_id =
    import_set_of_closures_id_for_pack pack_units pack
  in
  let import_function_declarations =
    import_function_declarations_for_pack pack_units pack
  in
  let sets_of_closures =
    Set_of_closures_id.Map.map_keys import_set_of_closures_id
      (Set_of_closures_id.Map.map
         import_function_declarations
         exp.sets_of_closures)
  in
  Export_info.create ~sets_of_closures
    ~closures:(Flambda_utils.make_closure_map' sets_of_closures)
    ~offset_fun:exp.offset_fun
    ~offset_fv:exp.offset_fv
    ~values:(import_eidmap import_descr exp.values)
    ~symbol_id:(Symbol.Map.map_keys import_sym
      (Symbol.Map.map import_eid exp.symbol_id))
    ~constant_sets_of_closures:
      (Set_of_closures_id.Set.map import_set_of_closures_id
         exp.constant_sets_of_closures)
    ~invariant_params:
      (Set_of_closures_id.Map.map_keys import_set_of_closures_id
         exp.invariant_params)

let clear_import_state () =
  Set_of_closures_id.Tbl.clear imported_function_declarations_table;
  Set_of_closures_id.Tbl.clear rename_set_of_closures_id_state;
  Export_id.Tbl.clear rename_id_state
