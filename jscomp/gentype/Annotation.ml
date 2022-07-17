type import = { name : string; importPath : ImportPath.t }

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
let tagIsBsAs s = s = "bs.as" || s = "as"
let tagIsBsInt s = s = "bs.int" || s = "int"
let tagIsBsString s = s = "bs.string" || s = "string"
let tagIsUnboxed s = s = "unboxed" || s = "ocaml.unboxed"
let tagIsGenTypeImport s = s = "genType.import" || s = "gentype.import"
let tagIsGenTypeOpaque s = s = "genType.opaque" || s = "gentype.opaque"

let tagIsOneOfTheGenTypeAnnotations s =
  tagIsGenType s || tagIsGenTypeAs s || tagIsGenTypeImport s
  || tagIsGenTypeOpaque s

let tagIsGenTypeIgnoreInterface s =
  s = "genType.ignoreInterface" || s = "gentype.ignoreInterface"

let tagIsOcamlDoc s = s = "ocaml.doc"
let tagIsInternLocal s = s = "internal.local"

let rec getAttributePayload checkText (attributes : Typedtree.attributes) =
  let rec fromExpr (expr : Parsetree.expression) =
    match expr with
    | { pexp_desc = Pexp_constant (Pconst_string (s, _)) } ->
        Some (StringPayload s)
    | { pexp_desc = Pexp_constant (Pconst_integer (n, _)) } ->
        Some (IntPayload n)
    | { pexp_desc = Pexp_constant (Pconst_float (s, _)) } ->
        Some (FloatPayload s)
    | {
     pexp_desc = Pexp_construct ({ txt = Lident (("true" | "false") as s) }, _);
     _;
    } ->
        Some (BoolPayload (s = "true"))
    | { pexp_desc = Pexp_tuple exprs } ->
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
    | { pexp_desc = Pexp_ident { txt } } -> Some (IdentPayload txt)
    | _ -> None
  in
  match attributes with
  | [] -> None
  | ({ Asttypes.txt }, payload) :: _tl when checkText txt -> (
      match payload with
      | PStr [] -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_eval (expr, _) } :: _) -> expr |> fromExpr
      | PStr ({ pstr_desc = Pstr_extension _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_value _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_primitive _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_type _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_typext _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_exception _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_module _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_recmodule _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_modtype _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_open _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_class _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_class_type _ } :: _) ->
          Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_include _ } :: _) -> Some UnrecognizedPayload
      | PStr ({ pstr_desc = Pstr_attribute _ } :: _) -> Some UnrecognizedPayload
      | PPat _ -> Some UnrecognizedPayload
      | PSig _ -> Some UnrecognizedPayload
      | PTyp _ -> Some UnrecognizedPayload)
  | _hd :: tl -> getAttributePayload checkText tl

let getGenTypeAsRenaming attributes =
  match attributes |> getAttributePayload tagIsGenTypeAs with
  | Some (StringPayload s) -> Some s
  | None -> (
      match attributes |> getAttributePayload tagIsGenType with
      | Some (StringPayload s) -> Some s
      | _ -> None)
  | _ -> None

let getBsAsRenaming attributes =
  match attributes |> getAttributePayload tagIsBsAs with
  | Some (StringPayload s) -> Some s
  | _ -> None

let getBsAsInt attributes =
  match attributes |> getAttributePayload tagIsBsAs with
  | Some (IntPayload s) -> (
      try Some (int_of_string s) with Failure _ -> None)
  | _ -> None

let getAttributeImportRenaming attributes =
  let attributeImport = attributes |> getAttributePayload tagIsGenTypeImport in
  let genTypeAsRenaming = attributes |> getGenTypeAsRenaming in
  match (attributeImport, genTypeAsRenaming) with
  | Some (StringPayload importString), _ ->
      (Some importString, genTypeAsRenaming)
  | ( Some
        (TuplePayload
          [ StringPayload importString; StringPayload renameString ]),
      _ ) ->
      (Some importString, Some renameString)
  | _ -> (None, genTypeAsRenaming)

let getDocString attributes =
  let docPayload = attributes |> getAttributePayload tagIsOcamlDoc in
  match docPayload with
  | Some (StringPayload docString) -> "/** " ^ docString ^ " */\n"
  | _ -> ""

let hasAttribute checkText (attributes : Typedtree.attributes) =
  getAttributePayload checkText attributes <> None

let fromAttributes ~loc (attributes : Typedtree.attributes) =
  if hasAttribute tagIsGenTypeOpaque attributes then GenTypeOpaque
  else if hasAttribute (fun s -> tagIsGenType s || tagIsGenTypeAs s) attributes
  then (
    (match attributes |> getAttributePayload tagIsGenType with
    | Some UnrecognizedPayload -> ()
    | Some _ ->
        Log_.Color.setup ();
        Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
            Format.fprintf ppf "Annotation payload is ignored")
    | _ -> ());
    GenType)
  else NoGenType

let rec moduleTypeCheckAnnotation ~checkAnnotation
    ({ mty_desc } : Typedtree.module_type) =
  match mty_desc with
  | Tmty_signature signature ->
      signature |> signatureCheckAnnotation ~checkAnnotation
  | Tmty_ident _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _ | Tmty_alias _
    ->
      false

and moduleDeclarationCheckAnnotation ~checkAnnotation
    ({ md_attributes; md_type; md_loc = loc } : Typedtree.module_declaration) =
  md_attributes |> checkAnnotation ~loc
  || md_type |> moduleTypeCheckAnnotation ~checkAnnotation

and signatureItemCheckAnnotation ~checkAnnotation
    (signatureItem : Typedtree.signature_item) =
  match signatureItem with
  | { Typedtree.sig_desc = Typedtree.Tsig_type (_, typeDeclarations) } ->
      typeDeclarations
      |> List.exists
           (fun ({ typ_attributes; typ_loc = loc } : Typedtree.type_declaration)
           -> typ_attributes |> checkAnnotation ~loc)
  | { sig_desc = Tsig_value { val_attributes; val_loc = loc } } ->
      val_attributes |> checkAnnotation ~loc
  | { sig_desc = Tsig_module moduleDeclaration } ->
      moduleDeclaration |> moduleDeclarationCheckAnnotation ~checkAnnotation
  | { sig_desc = Tsig_attribute attribute; sig_loc = loc } ->
      [ attribute ] |> checkAnnotation ~loc
  | _ -> false

and signatureCheckAnnotation ~checkAnnotation (signature : Typedtree.signature)
    =
  signature.sig_items
  |> List.exists (signatureItemCheckAnnotation ~checkAnnotation)

let rec structureItemCheckAnnotation ~checkAnnotation
    (structureItem : Typedtree.structure_item) =
  match structureItem with
  | { Typedtree.str_desc = Typedtree.Tstr_type (_, typeDeclarations) } ->
      typeDeclarations
      |> List.exists
           (fun ({ typ_attributes; typ_loc = loc } : Typedtree.type_declaration)
           -> typ_attributes |> checkAnnotation ~loc)
  | { str_desc = Tstr_value (_loc, valueBindings) } ->
      valueBindings
      |> List.exists
           (fun ({ vb_attributes; vb_loc = loc } : Typedtree.value_binding) ->
             vb_attributes |> checkAnnotation ~loc)
  | { str_desc = Tstr_primitive { val_attributes; val_loc = loc } } ->
      val_attributes |> checkAnnotation ~loc
  | { str_desc = Tstr_module moduleBinding } ->
      moduleBinding |> moduleBindingCheckAnnotation ~checkAnnotation
  | { str_desc = Tstr_recmodule moduleBindings } ->
      moduleBindings
      |> List.exists (moduleBindingCheckAnnotation ~checkAnnotation)
  | { str_desc = Tstr_include { incl_attributes; incl_mod; incl_loc = loc } } ->
      incl_attributes |> checkAnnotation ~loc
      || incl_mod |> moduleExprCheckAnnotation ~checkAnnotation
  | _ -> false

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
    ({ mb_expr; mb_attributes; mb_loc = loc } : Typedtree.module_binding) =
  mb_attributes |> checkAnnotation ~loc
  || mb_expr |> moduleExprCheckAnnotation ~checkAnnotation

and structureCheckAnnotation ~checkAnnotation (structure : Typedtree.structure)
    =
  structure.str_items
  |> List.exists (structureItemCheckAnnotation ~checkAnnotation)

let sanitizeVariableName name =
  name |> String.map (function '-' -> '_' | c -> c)

let importFromString importString : import =
  let name =
    let base = importString |> Filename.basename in
    (try base |> Filename.chop_extension with Invalid_argument _ -> base)
    |> sanitizeVariableName
  in
  let importPath = ImportPath.fromStringUnsafe importString in
  { name; importPath }
