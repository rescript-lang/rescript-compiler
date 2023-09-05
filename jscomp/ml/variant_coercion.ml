(* TODO: Improve error messages? Say why we can't coerce. *)

(* Right now we only allow coercing to primitives string/int/float *)
let can_coerce_path (path : Path.t) =
  Path.same path Predef.path_string
  || Path.same path Predef.path_int
  || Path.same path Predef.path_float

let can_coerce_variant ~(path : Path.t)
    (constructors : Types.constructor_declaration list) =
  constructors
  |> List.for_all (fun (c : Types.constructor_declaration) ->
         let args = c.cd_args in
         let payload = Ast_untagged_variants.process_tag_type c.cd_attributes in
         match args with
         | Cstr_tuple [] -> (
           match payload with
           | None | Some (String _) -> Path.same path Predef.path_string
           | Some (Int _) -> Path.same path Predef.path_int
           | Some (Float _) -> Path.same path Predef.path_float
           | Some (Null | Undefined | Bool _ | Untagged _) -> false)
         | _ -> false)

let can_try_coerce_variant_to_primitive
    ((_, p, typedecl) : Path.t * Path.t * Types.type_declaration) =
  match typedecl with
  | {type_kind = Type_variant constructors; type_params = []}
    when Path.name p <> "bool" ->
    (* bool is represented as a variant internally, so we need to account for that *)
    Some constructors
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

type variant_configuration_error =
  | Untagged of {left_is_unboxed: bool}
  | TagName of {left_tag: string option; right_tag: string option}

type variant_error =
  | VariantError of {
      left_loc: Location.t;
      right_loc: Location.t;
      error: variant_configuration_error;
      is_spread_context: bool;
    }

exception VariantConfigurationError of variant_error

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

let variant_configuration_can_be_coerced_raises ~is_spread_context ~left_loc
    ~right_loc ~(left_attributes : Parsetree.attributes)
    ~(right_attributes : Parsetree.attributes) =
  (match
     ( Ast_untagged_variants.process_untagged left_attributes,
       Ast_untagged_variants.process_untagged right_attributes )
   with
  | true, true | false, false -> ()
  | left, _right ->
    raise
      (VariantConfigurationError
         (VariantError
            {
              is_spread_context;
              left_loc;
              right_loc;
              error = Untagged {left_is_unboxed = left};
            })));

  match
    ( Ast_untagged_variants.process_tag_name left_attributes,
      Ast_untagged_variants.process_tag_name right_attributes )
  with
  | Some host_tag, Some spread_tag when host_tag = spread_tag -> ()
  | None, None -> ()
  | left_tag, right_tag ->
    raise
      (VariantConfigurationError
         (VariantError
            {
              is_spread_context;
              left_loc;
              right_loc;
              error = TagName {left_tag; right_tag};
            }))
