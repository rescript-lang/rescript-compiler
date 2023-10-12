type import = {importPath: ImportPath.t}

type attributePayload =
  | BoolPayload of bool
  | FloatPayload of string
  | IdentPayload of Longident.t
  | IntPayload of string
  | StringPayload of string
  | TuplePayload of attributePayload list
  | UnrecognizedPayload

type t = GenType | GenTypeOpaque | NoGenType

let toString annotation =
  match annotation with
  | GenType -> "GenType"
  | GenTypeOpaque -> "GenTypeOpaque"
  | NoGenType -> "NoGenType"

let tagIsGenType s = s = "genType" || s = "gentype"
let tagIsGenTypeAs s = s = "genType.as" || s = "gentype.as"
let tagIsAs s = s = "bs.as" || s = "as"
let tagIsInt s = s = "bs.int" || s = "int"
let tagIsString s = s = "bs.string" || s = "string"

let tagIsTag s = s = "tag"

let tagIsUnboxed s = s = "unboxed" || s = "ocaml.unboxed"
let tagIsGenTypeImport s = s = "genType.import" || s = "gentype.import"
let tagIsGenTypeOpaque s = s = "genType.opaque" || s = "gentype.opaque"

let tagIsOneOfTheGenTypeAnnotations s =
  tagIsGenType s || tagIsGenTypeAs s || tagIsGenTypeImport s
  || tagIsGenTypeOpaque s

let tagIsGenTypeIgnoreInterface s =
  s = "genType.ignoreInterface" || s = "gentype.ignoreInterface"

let tagIsDoc s =
  match s with
  | "ocaml.doc" | "res.doc" -> true
  | _ -> false
let tagIsInternLocal s = s = "internal.local"

let rec getAttributePayload checkText (attributes : Typedtree.attributes) =
  let rec fromExpr (expr : Parsetree.expression) =
    match expr with
    | {pexp_desc = Pexp_constant (Pconst_string (s, _))} ->
      Some (StringPayload s)
    | {pexp_desc = Pexp_constant (Pconst_integer (n, _))} -> Some (IntPayload n)
    | {pexp_desc = Pexp_constant (Pconst_float (s, _))} -> Some (FloatPayload s)
    | {
     pexp_desc = Pexp_construct ({txt = Lident (("true" | "false") as s)}, _);
     _;
    } ->
      Some (BoolPayload (s = "true"))
    | {pexp_desc = Pexp_tuple exprs} ->
      let payloads =
        exprs |> List.rev
        |> List.fold_left
             (fun payloads expr ->
               match expr |> fromExpr with
               | Some payload -> payload :: payloads
               | None -> payloads)
             []
      in
      Some (TuplePayload payloads)
    | {pexp_desc = Pexp_ident {txt}} -> Some (IdentPayload txt)
    | _ -> None
  in
  match attributes with
  | [] -> None
  | ({txt; loc}, payload) :: _tl when checkText txt -> (
    let payload =
      match payload with
      | PStr [] -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_eval (expr, _)} :: _) -> expr |> fromExpr
      | PStr ({pstr_desc = Pstr_extension _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_value _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_primitive _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_type _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_typext _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_exception _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_module _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_recmodule _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_modtype _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_open _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_class _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_class_type _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_include _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_attribute _} :: _) -> Some UnrecognizedPayload
      | PPat _ -> Some UnrecognizedPayload
      | PSig _ -> Some UnrecognizedPayload
      | PTyp _ -> Some UnrecognizedPayload
    in
    match payload with
    | None -> None
    | Some payload -> Some (loc, payload))
  | _hd :: tl -> getAttributePayload checkText tl

let getGenTypeAsRenaming attributes =
  match attributes |> getAttributePayload tagIsGenTypeAs with
  | Some (_, StringPayload s) -> Some s
  | None -> (
    match attributes |> getAttributePayload tagIsGenType with
    | Some (_, StringPayload s) -> Some s
    | _ -> None)
  | _ -> None

(* This is not supported anymore: only use to give a warning *)
let checkUnsupportedGenTypeAsRenaming attributes =
  let error ~loc =
    Log_.Color.setup ();
    Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
        Format.fprintf ppf
          "@\n\
           @genType.as is not supported anymore in type definitions. Use @as \
           from the language.")
  in
  match attributes |> getAttributePayload tagIsGenTypeAs with
  | Some (loc, _) -> error ~loc
  | None -> (
    match attributes |> getAttributePayload tagIsGenType with
    | Some (loc, _) -> error ~loc
    | None -> ())

let getAsString attributes =
  match attributes |> getAttributePayload tagIsAs with
  | Some (_, StringPayload s) -> Some s
  | _ -> None

let getAsInt attributes =
  match attributes |> getAttributePayload tagIsAs with
  | Some (_, IntPayload s) -> (
    try Some (int_of_string s) with Failure _ -> None)
  | _ -> None

let getAttributeImportRenaming attributes =
  let attributeImport = attributes |> getAttributePayload tagIsGenTypeImport in
  let genTypeAsRenaming = attributes |> getGenTypeAsRenaming in
  match (attributeImport, genTypeAsRenaming) with
  | Some (_, StringPayload importString), _ ->
    (Some importString, genTypeAsRenaming)
  | ( Some
        ( _,
          TuplePayload [StringPayload importString; StringPayload renameString]
        ),
      _ ) ->
    (Some importString, Some renameString)
  | _ -> (None, genTypeAsRenaming)

let getTag attributes =
  match attributes |> getAttributePayload tagIsTag with
  | Some (_, StringPayload s) -> Some s
  | _ -> None

let getDocPayload attributes =
  let docPayload = attributes |> getAttributePayload tagIsDoc in
  match docPayload with
  | Some (_, StringPayload docString) when docString <> "" -> Some docString
  | _ -> None

let docStringFromAttrs attributes = attributes |> getDocPayload

let hasAttribute checkText (attributes : Typedtree.attributes) =
  getAttributePayload checkText attributes <> None

let fromAttributes ~(config : GenTypeConfig.t) ~loc
    (attributes : Typedtree.attributes) =
  let default = if config.everything then GenType else NoGenType in
  if hasAttribute tagIsGenTypeOpaque attributes then GenTypeOpaque
  else if hasAttribute (fun s -> tagIsGenType s || tagIsGenTypeAs s) attributes
  then (
    (match attributes |> getAttributePayload tagIsGenType with
    | Some (_, UnrecognizedPayload) -> ()
    | Some _ ->
      Log_.Color.setup ();
      Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
          Format.fprintf ppf "Annotation payload is ignored")
    | _ -> ());
    GenType)
  else default

let rec moduleTypeCheckAnnotation ~checkAnnotation
    ({mty_desc} : Typedtree.module_type) =
  match mty_desc with
  | Tmty_signature signature ->
    signature |> signatureCheckAnnotation ~checkAnnotation
  | Tmty_ident _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _ | Tmty_alias _
    ->
    false

and moduleTypeDeclarationCheckAnnotation ~checkAnnotation
    ({mtd_type; mtd_attributes; mtd_loc = loc} :
      Typedtree.module_type_declaration) =
  mtd_attributes |> checkAnnotation ~loc
  ||
  match mtd_type with
  | None -> false
  | Some module_type ->
    module_type |> moduleTypeCheckAnnotation ~checkAnnotation

and moduleDeclarationCheckAnnotation ~checkAnnotation
    ({md_attributes; md_type; md_loc = loc} : Typedtree.module_declaration) =
  md_attributes |> checkAnnotation ~loc
  || md_type |> moduleTypeCheckAnnotation ~checkAnnotation

and signatureItemCheckAnnotation ~checkAnnotation
    (signatureItem : Typedtree.signature_item) =
  match signatureItem.sig_desc with
  | Tsig_type (_, typeDeclarations) ->
    typeDeclarations
    |> List.exists
         (fun ({typ_attributes; typ_loc = loc} : Typedtree.type_declaration) ->
           typ_attributes |> checkAnnotation ~loc)
  | Tsig_value {val_attributes; val_loc = loc} ->
    val_attributes |> checkAnnotation ~loc
  | Tsig_module moduleDeclaration ->
    moduleDeclaration |> moduleDeclarationCheckAnnotation ~checkAnnotation
  | Tsig_attribute attribute ->
    [attribute] |> checkAnnotation ~loc:signatureItem.sig_loc
  | Tsig_modtype moduleTypeDeclaration ->
    moduleTypeDeclaration
    |> moduleTypeDeclarationCheckAnnotation ~checkAnnotation
  | Tsig_typext _ | Tsig_exception _ | Tsig_recmodule _ | Tsig_open _
  | Tsig_include _ | Tsig_class _ | Tsig_class_type _ ->
    false

and signatureCheckAnnotation ~checkAnnotation (signature : Typedtree.signature)
    =
  signature.sig_items
  |> List.exists (signatureItemCheckAnnotation ~checkAnnotation)

let rec structureItemCheckAnnotation ~checkAnnotation
    (structureItem : Typedtree.structure_item) =
  match structureItem.str_desc with
  | Tstr_type (_, typeDeclarations) ->
    typeDeclarations
    |> List.exists
         (fun ({typ_attributes; typ_loc = loc} : Typedtree.type_declaration) ->
           typ_attributes |> checkAnnotation ~loc)
  | Tstr_value (_loc, valueBindings) ->
    valueBindings
    |> List.exists
         (fun ({vb_attributes; vb_loc = loc} : Typedtree.value_binding) ->
           vb_attributes |> checkAnnotation ~loc)
  | Tstr_primitive {val_attributes; val_loc = loc} ->
    val_attributes |> checkAnnotation ~loc
  | Tstr_module moduleBinding ->
    moduleBinding |> moduleBindingCheckAnnotation ~checkAnnotation
  | Tstr_recmodule moduleBindings ->
    moduleBindings
    |> List.exists (moduleBindingCheckAnnotation ~checkAnnotation)
  | Tstr_include {incl_attributes; incl_mod; incl_loc = loc} ->
    incl_attributes |> checkAnnotation ~loc
    || incl_mod |> moduleExprCheckAnnotation ~checkAnnotation
  | Tstr_modtype moduleTypeDeclaration ->
    moduleTypeDeclaration
    |> moduleTypeDeclarationCheckAnnotation ~checkAnnotation
  | Tstr_attribute attribute ->
    [attribute] |> checkAnnotation ~loc:structureItem.str_loc
  | Tstr_eval _ | Tstr_typext _ | Tstr_exception _ | Tstr_open _ | Tstr_class _
  | Tstr_class_type _ ->
    false

and moduleExprCheckAnnotation ~checkAnnotation
    (moduleExpr : Typedtree.module_expr) =
  match moduleExpr.mod_desc with
  | Tmod_structure structure ->
    structure |> structureCheckAnnotation ~checkAnnotation
  | Tmod_constraint
      (moduleExpr, _moduleType, moduleTypeConstraint, _moduleCoercion) -> (
    moduleExpr |> moduleExprCheckAnnotation ~checkAnnotation
    ||
    match moduleTypeConstraint with
    | Tmodtype_explicit moduleType ->
      moduleType |> moduleTypeCheckAnnotation ~checkAnnotation
    | Tmodtype_implicit -> false)
  | Tmod_ident _ | Tmod_functor _ | Tmod_apply _ | Tmod_unpack _ -> false

and moduleBindingCheckAnnotation ~checkAnnotation
    ({mb_expr; mb_attributes; mb_loc = loc} : Typedtree.module_binding) =
  mb_attributes |> checkAnnotation ~loc
  || mb_expr |> moduleExprCheckAnnotation ~checkAnnotation

and structureCheckAnnotation ~checkAnnotation (structure : Typedtree.structure)
    =
  structure.str_items
  |> List.exists (structureItemCheckAnnotation ~checkAnnotation)

let importFromString importString : import =
  let importPath = ImportPath.fromStringUnsafe importString in
  {importPath}

let updateConfigForModule ~(config : GenTypeConfig.t) attributes =
  if attributes |> hasAttribute tagIsGenType then
    {config with everything = true}
  else config
