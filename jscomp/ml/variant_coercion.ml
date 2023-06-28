(* TODO: Improve error messages? Say why we can't coerce. *)

let check_constructors (constructors : Types.constructor_declaration list) check
    =
  List.for_all
    (fun (c : Types.constructor_declaration) ->
      check c.cd_args (Ast_untagged_variants.process_tag_type c.cd_attributes))
    constructors

let can_coerce_to_string (constructors : Types.constructor_declaration list) =
  check_constructors constructors (fun args payload ->
      match (args, payload) with
      | Cstr_tuple [], (None | Some (String _)) -> true
      | _ -> false)

let can_coerce_to_int (constructors : Types.constructor_declaration list) =
  check_constructors constructors (fun args payload ->
      match (args, payload) with
      | Cstr_tuple [], Some (Int _) -> true
      | _ -> false)

let can_coerce_to_float (constructors : Types.constructor_declaration list) =
  check_constructors constructors (fun args payload ->
      match (args, payload) with
      | Cstr_tuple [], Some (Float _) -> true
      | _ -> false)

let can_coerce_path (path : Path.t) =
  Path.same path Predef.path_string
  || Path.same path Predef.path_int
  || Path.same path Predef.path_float

let can_coerce_variant ~(path : Path.t)
    (constructors : Types.constructor_declaration list) =
  if Path.same path Predef.path_string && can_coerce_to_string constructors then
    true
  else if Path.same path Predef.path_int && can_coerce_to_int constructors then
    true
  else if Path.same path Predef.path_float && can_coerce_to_float constructors
  then true
  else false

let is_variant_typedecl
    ((_, _, typedecl) : Path.t * Path.t * Types.type_declaration) =
  match typedecl with
  | {type_kind = Type_variant constructors} -> Some constructors
  | _ -> None

let variant_representation_matches (c1_attrs : Parsetree.attributes)
    (c2_attrs : Parsetree.attributes) =
  match
    ( Ast_untagged_variants.process_tag_type c1_attrs,
      Ast_untagged_variants.process_tag_type c2_attrs )
  with
  | None, None -> true
  | Some s1, Some s2 when s1 = s2 -> true
  | _ -> false

let variant_configuration_can_be_coerced (a1 : Parsetree.attributes)
    (a2 : Parsetree.attributes) =
  let unboxed =
    match
      ( Ast_untagged_variants.process_untagged a1,
        Ast_untagged_variants.process_untagged a2 )
    with
    | true, true | false, false -> true
    | _ -> false
  in
  if not unboxed then false
  else
    let tag =
      match
        ( Ast_untagged_variants.process_tag_name a1,
          Ast_untagged_variants.process_tag_name a2 )
      with
      | Some tag1, Some tag2 when tag1 = tag2 -> true
      | None, None -> true
      | _ -> false
    in
    if not tag then false else true
