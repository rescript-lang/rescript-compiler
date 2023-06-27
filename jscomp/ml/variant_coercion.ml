let find_as_attribute_payload (attributes : Parsetree.attribute list) =
  attributes
  |> List.find_map (fun (attr : Parsetree.attribute) ->
         match attr with
         | {txt = "as"}, payload -> Some payload
         | _ -> None)

(* TODO: Improve error messages? Say why we can't coerce. *)

let can_coerce_to_string (constructors : Types.constructor_declaration list) =
  List.for_all
    (fun (c : Types.constructor_declaration) ->
      match (c.cd_args, find_as_attribute_payload c.cd_attributes) with
      | Cstr_tuple [], None -> true
      | Cstr_tuple [], Some payload
        when Ast_payload.is_single_string payload |> Option.is_some ->
        true
      | _ -> false)
    constructors

let can_coerce_to_int (constructors : Types.constructor_declaration list) =
  List.for_all
    (fun (c : Types.constructor_declaration) ->
      match (c.cd_args, find_as_attribute_payload c.cd_attributes) with
      | Cstr_tuple [], Some payload
        when Ast_payload.is_single_int payload |> Option.is_some ->
        true
      | _ -> false)
    constructors

let can_coerce_to_float (constructors : Types.constructor_declaration list) =
  List.for_all
    (fun (c : Types.constructor_declaration) ->
      match (c.cd_args, find_as_attribute_payload c.cd_attributes) with
      | Cstr_tuple [], Some payload
        when Ast_payload.is_single_float payload |> Option.is_some ->
        true
      | _ -> false)
    constructors

let can_coerce_path (path : Path.t) =
  Path.same path Predef.path_string
  || Path.same path Predef.path_int
  || Path.same path Predef.path_float

let can_coerce_variant ~(path : Path.t)
    (constructors : Types.constructor_declaration list) =
  if can_coerce_path path then
    if Path.same path Predef.path_string && can_coerce_to_string constructors
    then true
    else if Path.same path Predef.path_int && can_coerce_to_int constructors
    then true
    else if Path.same path Predef.path_float && can_coerce_to_float constructors
    then true
    else false
  else false
