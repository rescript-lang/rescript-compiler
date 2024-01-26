open GenTypeCommon

type declarationKind =
  | RecordDeclarationFromTypes of
      Types.label_declaration list * Types.record_representation
  | GeneralDeclaration of Typedtree.core_type option
  | GeneralDeclarationFromTypes of Types.type_expr option
      (** As the above, but from Types not Typedtree *)
  | VariantDeclarationFromTypes of Types.constructor_declaration list
  | NoDeclaration

let createExportTypeFromTypeDeclaration ~annotation ~loc ~nameAs ~opaque ~type_
    ~typeEnv ~docString typeName ~typeVars : CodeItem.exportFromTypeDeclaration
    =
  let resolvedTypeName =
    typeName |> sanitizeTypeName |> TypeEnv.addModulePath ~typeEnv
  in
  {
    exportType =
      {loc; nameAs; opaque; type_; typeVars; resolvedTypeName; docString};
    annotation;
  }

let createCase (label, attributes) ~poly =
  {
    labelJS =
      (match
         attributes |> Annotation.getAttributePayload Annotation.tagIsAs
       with
      | Some (_, IdentPayload (Lident "null")) -> NullLabel
      | Some (_, IdentPayload (Lident "undefined")) -> UndefinedLabel
      | Some (_, BoolPayload b) -> BoolLabel b
      | Some (_, FloatPayload s) -> FloatLabel s
      | Some (_, IntPayload i) -> IntLabel i
      | Some (_, StringPayload asLabel) -> StringLabel asLabel
      | _ ->
        if poly && isNumber label then IntLabel label else StringLabel label);
  }

(**
 * Rename record fields.
 * If @genType.as is used, perform renaming conversion.
 * If @bs.as is used (with records-as-objects active), escape and quote if
 * the identifier contains characters which are invalid as JS property names.
*)
let renameRecordField ~attributes ~name =
  attributes |> Annotation.checkUnsupportedGenTypeAsRenaming;
  match attributes |> Annotation.getAsString with
  | Some s -> s |> String.escaped
  | None -> name

let traslateDeclarationKind ~config ~loc ~outputFileRelative ~resolver
    ~typeAttributes ~typeEnv ~typeName ~typeVars declarationKind :
    CodeItem.typeDeclaration list =
  let docString = typeAttributes |> Annotation.docStringFromAttrs in
  let annotation = typeAttributes |> Annotation.fromAttributes ~config ~loc in
  let opaque =
    match annotation = Annotation.GenTypeOpaque with
    | true -> Some true
    | false -> None
    (* one means don't know *)
  in
  let importStringOpt, nameAs =
    typeAttributes |> Annotation.getAttributeImportRenaming
  in
  let unboxedAnnotation =
    typeAttributes |> Annotation.hasAttribute Annotation.tagIsUnboxed
  in
  let tagAnnotation = typeAttributes |> Annotation.getTag in
  let returnTypeDeclaration (typeDeclaration : CodeItem.typeDeclaration) =
    match opaque = Some true with
    | true -> [{typeDeclaration with importTypes = []}]
    | false -> [typeDeclaration]
  in
  let handleGeneralDeclaration
      (translation : TranslateTypeExprFromTypes.translation) =
    let exportFromTypeDeclaration =
      typeName
      |> createExportTypeFromTypeDeclaration ~annotation ~loc ~nameAs ~opaque
           ~type_:translation.type_ ~typeEnv ~docString ~typeVars
    in
    let importTypes =
      translation.dependencies
      |> Translation.translateDependencies ~config ~outputFileRelative ~resolver
    in
    {CodeItem.importTypes; exportFromTypeDeclaration}
  in
  let translateLabelDeclarations ?(inline = false) ~recordRepresentation
      labelDeclarations =
    let isOptional l =
      match recordRepresentation with
      | Types.Record_optional_labels lbls -> List.mem l lbls
      | _ -> false
    in
    let fieldTranslations =
      labelDeclarations
      |> List.map (fun {Types.ld_id; ld_mutable; ld_type; ld_attributes} ->
             let name =
               renameRecordField ~attributes:ld_attributes
                 ~name:(ld_id |> Ident.name)
             in
             let mutability =
               match ld_mutable = Mutable with
               | true -> Mutable
               | false -> Immutable
             in
             ( name,
               mutability,
               ld_type
               |> TranslateTypeExprFromTypes.translateTypeExprFromTypes ~config
                    ~typeEnv,
               Annotation.docStringFromAttrs ld_attributes ))
    in
    let dependencies =
      fieldTranslations
      |> List.map (fun (_, _, {TranslateTypeExprFromTypes.dependencies}, _) ->
             dependencies)
      |> List.concat
    in
    let fields =
      fieldTranslations
      |> List.map
           (fun
             (name, mutable_, {TranslateTypeExprFromTypes.type_}, docString) ->
             let optional, type1 =
               match type_ with
               | Option type1 when isOptional name -> (Optional, type1)
               | _ -> (Mandatory, type_)
             in
             {mutable_; nameJS = name; optional; type_ = type1; docString})
    in
    let type_ =
      match fields with
      | [field] when unboxedAnnotation -> field.type_
      | _ -> Object ((if inline then Inline else Closed), fields)
    in
    {TranslateTypeExprFromTypes.dependencies; type_}
  in
  match (declarationKind, importStringOpt) with
  | _, Some importString ->
    (* import type *)
    let typeName_ = typeName in
    let nameWithModulePath =
      typeName_ |> TypeEnv.addModulePath ~typeEnv |> ResolvedName.toString
    in
    let typeName, asTypeName =
      match nameAs with
      | Some asString -> (asString, "$$" ^ nameWithModulePath)
      | None -> (nameWithModulePath, "$$" ^ nameWithModulePath)
    in
    let importTypes =
      [
        {
          CodeItem.typeName;
          asTypeName = Some asTypeName;
          importPath = importString |> ImportPath.fromStringUnsafe;
        };
      ]
    in
    let exportFromTypeDeclaration =
      (* Make the imported type usable from other modules by exporting it too. *)
      typeName_
      |> createExportTypeFromTypeDeclaration ~docString ~annotation:GenType ~loc
           ~nameAs:None ~opaque:(Some false)
           ~type_:
             (asTypeName
             |> ident ~typeArgs:(typeVars |> List.map (fun s -> TypeVar s)))
           ~typeEnv ~typeVars
    in
    [{CodeItem.importTypes; exportFromTypeDeclaration}]
  | (GeneralDeclarationFromTypes None | GeneralDeclaration None), None ->
    {
      CodeItem.importTypes = [];
      exportFromTypeDeclaration =
        typeName
        |> createExportTypeFromTypeDeclaration ~docString ~annotation ~loc
             ~nameAs ~opaque:(Some true) ~type_:unknown ~typeEnv ~typeVars;
    }
    |> returnTypeDeclaration
  | GeneralDeclarationFromTypes (Some typeExpr), None ->
    let translation =
      typeExpr
      |> TranslateTypeExprFromTypes.translateTypeExprFromTypes ~config ~typeEnv
    in
    translation |> handleGeneralDeclaration |> returnTypeDeclaration
  | GeneralDeclaration (Some coreType), None ->
    let translation =
      coreType |> TranslateCoreType.translateCoreType ~config ~typeEnv
    in
    let type_ =
      match (coreType, translation.type_) with
      | {ctyp_desc = Ttyp_variant (rowFields, _, _)}, Variant variant ->
        let rowFieldsVariants = rowFields |> TranslateCoreType.processVariant in
        let noPayloads =
          rowFieldsVariants.noPayloads |> List.map (createCase ~poly:true)
        in
        let payloads =
          if
            variant.payloads |> List.length
            = (rowFieldsVariants.payloads |> List.length)
          then
            (List.combine variant.payloads rowFieldsVariants.payloads
             [@doesNotRaise])
            |> List.map (fun (payload, (label, attributes, _)) ->
                   let case = (label, attributes) |> createCase ~poly:true in
                   {payload with case})
          else variant.payloads
        in
        createVariant ~inherits:variant.inherits ~noPayloads ~payloads
          ~polymorphic:true ~tag:None ~unboxed:false
      | _ -> translation.type_
    in
    {translation with type_} |> handleGeneralDeclaration
    |> returnTypeDeclaration
  | RecordDeclarationFromTypes (labelDeclarations, recordRepresentation), None
    ->
    let {TranslateTypeExprFromTypes.dependencies; type_} =
      labelDeclarations |> translateLabelDeclarations ~recordRepresentation
    in
    let importTypes =
      dependencies
      |> Translation.translateDependencies ~config ~outputFileRelative ~resolver
    in
    {
      CodeItem.importTypes;
      exportFromTypeDeclaration =
        typeName
        |> createExportTypeFromTypeDeclaration ~docString ~annotation ~loc
             ~nameAs ~opaque ~type_ ~typeEnv ~typeVars;
    }
    |> returnTypeDeclaration
  | VariantDeclarationFromTypes constructorDeclarations, None ->
    let variants =
      constructorDeclarations
      |> List.map (fun constructorDeclaration ->
             let constructorArgs = constructorDeclaration.Types.cd_args in
             let attributes = constructorDeclaration.cd_attributes in
             let name = constructorDeclaration.cd_id |> Ident.name in
             let argsTranslation =
               match constructorArgs with
               | Cstr_tuple typeExprs ->
                 typeExprs
                 |> TranslateTypeExprFromTypes.translateTypeExprsFromTypes
                      ~config ~typeEnv
               | Cstr_record labelDeclarations ->
                 [
                   labelDeclarations
                   |> translateLabelDeclarations ~inline:true
                        ~recordRepresentation:Types.Record_regular;
                 ]
             in
             let argTypes =
               argsTranslation
               |> List.map (fun {TranslateTypeExprFromTypes.type_} -> type_)
             in
             let importTypes =
               argsTranslation
               |> List.map (fun {TranslateTypeExprFromTypes.dependencies} ->
                      dependencies)
               |> List.concat
               |> Translation.translateDependencies ~config ~outputFileRelative
                    ~resolver
             in
             (name, attributes, argTypes, importTypes))
    in
    let variantsNoPayload, variantsWithPayload =
      variants |> List.partition (fun (_, _, argTypes, _) -> argTypes = [])
    in
    let noPayloads =
      variantsNoPayload
      |> List.map (fun (name, attributes, _argTypes, _importTypes) ->
             (name, attributes) |> createCase ~poly:false)
    in
    let payloads =
      variantsWithPayload
      |> List.map (fun (name, attributes, argTypes, _importTypes) ->
             let type_ =
               match argTypes with
               | [type_] -> type_
               | _ -> Tuple argTypes
             in
             {case = (name, attributes) |> createCase ~poly:false; t = type_})
    in
    let variantTyp =
      createVariant ~inherits:[] ~noPayloads ~payloads ~polymorphic:false
        ~tag:tagAnnotation ~unboxed:unboxedAnnotation
    in
    let resolvedTypeName = typeName |> TypeEnv.addModulePath ~typeEnv in
    let exportFromTypeDeclaration =
      {
        CodeItem.exportType =
          {
            loc;
            nameAs;
            opaque;
            type_ = variantTyp;
            typeVars;
            resolvedTypeName;
            docString;
          };
        annotation;
      }
    in
    let importTypes =
      variants
      |> List.map (fun (_, _, _, importTypes) -> importTypes)
      |> List.concat
    in
    {CodeItem.exportFromTypeDeclaration; importTypes} |> returnTypeDeclaration
  | NoDeclaration, None -> []

let hasSomeGADTLeaf constructorDeclarations =
  List.exists
    (fun declaration -> declaration.Types.cd_res != None)
    constructorDeclarations

let translateTypeDeclaration ~config ~outputFileRelative ~resolver ~typeEnv
    ({typ_attributes; typ_id; typ_loc; typ_manifest; typ_params; typ_type} :
      Typedtree.type_declaration) : CodeItem.typeDeclaration list =
  if !Debug.translation then
    Log_.item "Translate Type Declaration %s\n" (typ_id |> Ident.name);

  let typeName = Ident.name typ_id in
  let typeVars =
    typ_params
    |> List.map (fun (coreType, _) -> coreType)
    |> TypeVars.extractFromCoreType
  in
  let declarationKind =
    match typ_type.type_kind with
    | Type_record (labelDeclarations, recordRepresentation) ->
      RecordDeclarationFromTypes (labelDeclarations, recordRepresentation)
    | Type_variant constructorDeclarations ->
      VariantDeclarationFromTypes constructorDeclarations
    | Type_abstract -> GeneralDeclaration typ_manifest
    | _ -> NoDeclaration
  in
  declarationKind
  |> traslateDeclarationKind ~config ~loc:typ_loc ~outputFileRelative ~resolver
       ~typeAttributes:typ_attributes ~typeEnv ~typeName ~typeVars

let addTypeDeclarationIdToTypeEnv ~typeEnv
    ({typ_id} : Typedtree.type_declaration) =
  typeEnv |> TypeEnv.newType ~name:(typ_id |> Ident.name)

let translateTypeDeclarations ~config ~outputFileRelative ~recursive ~resolver
    ~typeEnv (typeDeclarations : Typedtree.type_declaration list) :
    CodeItem.typeDeclaration list =
  if recursive then
    typeDeclarations |> List.iter (addTypeDeclarationIdToTypeEnv ~typeEnv);
  typeDeclarations
  |> List.map (fun typeDeclaration ->
         let res =
           typeDeclaration
           |> translateTypeDeclaration ~config ~outputFileRelative ~resolver
                ~typeEnv
         in
         if not recursive then
           typeDeclaration |> addTypeDeclarationIdToTypeEnv ~typeEnv;
         res)
  |> List.concat
