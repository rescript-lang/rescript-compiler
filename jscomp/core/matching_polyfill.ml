(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let is_nullary_variant (x : Types.constructor_arguments) =
  match x with Types.Cstr_tuple [] -> true | _ -> false

let checkUntaggedVariant ~(blocks : (Location.t * Lambda.block) list) =
  let arrays = ref 0 in
  let objects = ref 0 in
  let unknowns = ref 0 in
  let invariant loc =
    if !unknowns <> 0 && (List.length blocks <> 1)
      then Bs_syntaxerr.err loc InvalidUntaggedVariantDefinition;
    if !objects > 1 || !arrays > 1
      then Bs_syntaxerr.err loc InvalidUntaggedVariantDefinition;
    () in
  Ext_list.rev_iter blocks (fun (loc, block) -> match block.block_type with
    | Some Unknown ->
      incr unknowns;
      invariant loc
    | Some Object ->
      incr objects;
      invariant loc
    | Some Array ->
      incr arrays;
      invariant loc
    | _ -> ())
  
let names_from_construct_pattern (pat : Typedtree.pattern) =
  let names_from_type_variant (cstrs : Types.constructor_declaration list) =
    let get_cstr_name (cstr: Types.constructor_declaration) =
      { Lambda.name = Ident.name cstr.cd_id;
        literal = Ast_attributes.process_as_value cstr.cd_attributes } in
    let get_tag_name (cstr: Types.constructor_declaration) =
      Ast_attributes.process_tag_name cstr.cd_attributes in
    let get_untagged (cstr: Types.constructor_declaration) =
      match Ast_attributes.process_untagged cstr.cd_attributes, cstr.cd_args with
      | false, _ -> None
      | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when Path.same path Predef.path_string ->
          Some Lambda.StringType
      | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when Path.same path Predef.path_int ->
          Some IntType
      | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when Path.same path Predef.path_float ->
          Some FloatType
      | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when Path.same path Predef.path_array ->
          Some Array
      | true, Cstr_tuple (_ :: _ :: _) ->
          (* C(_, _) with at least 2 args is an object *)
          Some Object
      | true, Cstr_record _ ->
          (* inline record is an object *)
          Some Object
      | true, Cstr_tuple [_] ->
          (* Every other single payload is unknown *)
          Some Unknown
      | true, _ -> None (* TODO: add restrictions here *)
    in
    let get_block cstr : Lambda.block =
      {cstr_name = get_cstr_name cstr; tag_name = get_tag_name cstr; block_type = get_untagged cstr} in
    let consts, blocks =
      Ext_list.fold_left cstrs ([], []) (fun (consts, blocks) cstr ->
          if is_nullary_variant cstr.cd_args then
            (get_cstr_name cstr :: consts, blocks)
          else (consts, (cstr.cd_loc, get_block cstr) :: blocks))
    in
    checkUntaggedVariant ~blocks;
    let blocks = blocks |> List.map snd in
    let consts = Ext_array.reverse_of_list consts in
    let blocks = Ext_array.reverse_of_list blocks in
    Some { Lambda.consts; blocks }
  in
  let rec resolve_path n (path : Path.t) =
    match Env.find_type path pat.pat_env with
    | { type_kind = Type_variant cstrs; _ } -> names_from_type_variant cstrs
    | { type_kind = Type_abstract; type_manifest = Some t; _ } -> (
        match (Ctype.unalias t).desc with
        | Tconstr (pathn, _, _) ->
            resolve_path (n + 1) pathn
        | _ -> None)
    | { type_kind = Type_abstract; type_manifest = None; _ } -> None
    | { type_kind = Type_record _ | Type_open (* Exceptions *); _ } -> None
  in

  match (Btype.repr pat.pat_type).desc with
  | Tconstr (path, _, _) -> resolve_path 0 path
  | _ -> assert false

(**
    Note it is a bit tricky when there is unbound var, 
    its type will be Tvar which is too complicated to support subtyping
*)
let variant_is_subtype (env : Env.t) (row_desc : Types.row_desc)
    (ty : Types.type_expr) : bool =
  match row_desc with
  | {
   row_closed = true;
   row_fixed = _;
   row_fields = (name, (Rabsent | Rpresent None)) :: rest;
  } ->
      if Ext_string.is_valid_hash_number name then
        Ext_list.for_all rest (function
          | name, (Rabsent | Rpresent None) ->
              Ext_string.is_valid_hash_number name
          | _ -> false)
        && Typeopt.is_base_type env ty Predef.path_int
      else
        Ext_list.for_all rest (function
          | name, (Rabsent | Rpresent None) ->
              not (Ext_string.is_valid_hash_number name)
          | _ -> false)
        && Typeopt.is_base_type env ty Predef.path_string
  | _ -> false
