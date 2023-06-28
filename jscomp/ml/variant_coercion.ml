let find_attribute_payload name (attributes : Parsetree.attribute list) =
  attributes
  |> List.find_map (fun (attr : Parsetree.attribute) ->
         match attr with
         | {txt}, payload when txt = name -> Some payload
         | _ -> None)

let find_as_attribute_payload (attributes : Parsetree.attribute list) =
  find_attribute_payload "as" attributes

(* TODO: Improve error messages? Say why we can't coerce. *)

let check_constructors (constructors : Types.constructor_declaration list) check
    =
  List.for_all
    (fun (c : Types.constructor_declaration) ->
      check c.cd_args (find_as_attribute_payload c.cd_attributes))
    constructors

let can_coerce_to_string (constructors : Types.constructor_declaration list) =
  check_constructors constructors (fun args payload ->
      match (args, payload) with
      | Cstr_tuple [], None -> true
      | Cstr_tuple [], Some payload
        when Ast_payload.is_single_string payload |> Option.is_some ->
        true
      | _ -> false)

let can_coerce_to_int (constructors : Types.constructor_declaration list) =
  check_constructors constructors (fun args payload ->
      match (args, payload) with
      | Cstr_tuple [], Some payload
        when Ast_payload.is_single_int payload |> Option.is_some ->
        true
      | _ -> false)

let can_coerce_to_float (constructors : Types.constructor_declaration list) =
  check_constructors constructors (fun args payload ->
      match (args, payload) with
      | Cstr_tuple [], Some payload
        when Ast_payload.is_single_float payload |> Option.is_some ->
        true
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

let find_attribute_payload_as_string name attrs =
  match find_attribute_payload name attrs with
  | None -> None
  | Some payload -> Ast_payload.is_single_string payload

let variant_representation_matches (c1_attrs : Parsetree.attributes)
    (c2_attrs : Parsetree.attributes) =
  match
    (find_as_attribute_payload c1_attrs, find_as_attribute_payload c2_attrs)
  with
  | None, None -> true
  | Some p1, Some p2 -> (
    let string_matches = match
      (Ast_payload.is_single_string p1, Ast_payload.is_single_string p2)
    with
    | Some (a, _), Some (b, _) when a = b -> true
    | _ -> false in
    if string_matches then true else
    let float_matches = match
      (Ast_payload.is_single_float p1, Ast_payload.is_single_float p2)
    with
    | Some a, Some b when a = b -> true
    | _ -> false in
    if float_matches then true else
    let int_matches = match
      (Ast_payload.is_single_int p1, Ast_payload.is_single_int p2)
    with
    | Some a, Some b when a = b -> true
    | _ -> false in
    if int_matches then true else
    false)
  | _ -> false

let variant_configuration_can_be_coerced (a1 : Parsetree.attributes)
    (a2 : Parsetree.attributes) =
  let unboxed =
    match
      (find_attribute_payload "unboxed" a1, find_attribute_payload "unboxed" a2)
    with
    | Some (PStr []), Some (PStr []) -> true
    | None, None -> true
    | _ -> false
  in
  if not unboxed then false
  else
    let tag =
      match
        ( find_attribute_payload_as_string "tag" a1,
          find_attribute_payload_as_string "tag" a2 )
      with
      | Some (tag1, _), Some (tag2, _) when tag1 = tag2 -> true
      | None, None -> true
      | _ -> false
    in
    if not tag then false else true