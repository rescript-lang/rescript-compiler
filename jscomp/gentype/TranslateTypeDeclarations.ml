open GenTypeCommon

type declarationKind =
  | RecordDeclarationFromTypes of Types.label_declaration list
  | GeneralDeclaration of Typedtree.core_type option
  | GeneralDeclarationFromTypes of Types.type_expr option
      (** As the above, but from Types not Typedtree *)
  | VariantDeclarationFromTypes of Types.constructor_declaration list
  | NoDeclaration

let createExportTypeFromTypeDeclaration ~annotation ~loc ~nameAs ~opaque ~type_
    ~typeEnv typeName ~typeVars : CodeItem.exportFromTypeDeclaration =
  let resolvedTypeName =
    typeName |> sanitizeTypeName |> TypeEnv.addModulePath ~typeEnv
  in
  {
    exportType = { loc; nameAs; opaque; type_; typeVars; resolvedTypeName };
    annotation;
  }

let createCase (label, attributes) =
  match
    attributes |> Annotation.getAttributePayload Annotation.tagIsGenTypeAs
  with
  | Some (BoolPayload b) -> { label; labelJS = BoolLabel b }
  | Some (FloatPayload s) -> { label; labelJS = FloatLabel s }
  | Some (IntPayload i) -> { label; labelJS = IntLabel i }
  | Some (StringPayload asLabel) -> { label; labelJS = StringLabel asLabel }
  | _ -> { label; labelJS = StringLabel label }

(**
 * Rename record fields.
 * If @genType.as is used, perform renaming conversion.
 * If @bs.as is used (with records-as-objects active), escape and quote if
 * the identifier contains characters which are invalid as JS property names.
*)
let renameRecordField ~attributes ~nameRE =
  match attributes |> Annotation.getGenTypeAsRenaming with
  | Some nameJS -> (nameJS, nameRE)
  | None -> (
      match attributes |> Annotation.getBsAsRenaming with
      | Some nameBS ->
          let escapedName = nameBS |> String.escaped in
          (escapedName, escapedName)
      | None -> (nameRE, nameRE))

let traslateDeclarationKind ~config ~loc ~outputFileRelative ~resolver
    ~typeAttributes ~typeEnv ~typeName ~typeVars declarationKind :
    CodeItem.typeDeclaration list =
  let annotation = typeAttributes |> Annotation.fromAttributes ~loc in
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
  let returnTypeDeclaration (typeDeclaration : CodeItem.typeDeclaration) =
    match opaque = Some true with
    | true -> [ { typeDeclaration with importTypes = [] } ]
    | false -> [ typeDeclaration ]
  in
  let handleGeneralDeclaration
      (translation : TranslateTypeExprFromTypes.translation) =
    let exportFromTypeDeclaration =
      typeName
      |> createExportTypeFromTypeDeclaration ~annotation ~loc ~nameAs ~opaque
           ~type_:translation.type_ ~typeEnv ~typeVars
    in
    let importTypes =
      translation.dependencies
      |> Translation.translateDependencies ~config ~outputFileRelative ~resolver
    in
    { CodeItem.importTypes; exportFromTypeDeclaration }
  in
  let translateLabelDeclarations labelDeclarations =
    let fieldTranslations =
      labelDeclarations
      |> List.map (fun { Types.ld_id; ld_mutable; ld_type; ld_attributes } ->
             let nameJS, nameRE =
               renameRecordField ~attributes:ld_attributes
                 ~nameRE:(ld_id |> Ident.name)
             in
             let mutability =
               match ld_mutable = Mutable with
               | true -> Mutable
               | false -> Immutable
             in
             ( nameJS,
               nameRE,
               mutability,
               ld_type
               |> TranslateTypeExprFromTypes.translateTypeExprFromTypes ~config
                    ~typeEnv ))
    in
    let dependencies =
      fieldTranslations
      |> List.map (fun (_, _, _, { TranslateTypeExprFromTypes.dependencies }) ->
             dependencies)
      |> List.concat
    in
    let fields =
      fieldTranslations
      |> List.map
           (fun (nameJS, nameRE, mutable_, { TranslateTypeExprFromTypes.type_ })
           ->
             let optional, type1 =
               match type_ with
               | Option type1 -> (Optional, type1)
               | _ -> (Mandatory, type_)
             in
             { mutable_; nameJS; nameRE; optional; type_ = type1 })
    in
    let type_ =
      match fields with
      | [ field ] when unboxedAnnotation -> field.type_
      | _ -> Object (Closed, fields)
    in
    { TranslateTypeExprFromTypes.dependencies; type_ }
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
        |> createExportTypeFromTypeDeclaration ~annotation:GenType ~loc
             ~nameAs:None ~opaque:(Some false)
             ~type_:
               (asTypeName
               |> ident ~typeArgs:(typeVars |> List.map (fun s -> TypeVar s)))
             ~typeEnv ~typeVars
      in
      [ { CodeItem.importTypes; exportFromTypeDeclaration } ]
  | (GeneralDeclarationFromTypes None | GeneralDeclaration None), None ->
      {
        CodeItem.importTypes = [];
        exportFromTypeDeclaration =
          typeName
          |> createExportTypeFromTypeDeclaration ~annotation ~loc ~nameAs
               ~opaque:(Some true) ~type_:unknown ~typeEnv ~typeVars;
      }
      |> returnTypeDeclaration
  | GeneralDeclarationFromTypes (Some typeExpr), None ->
      let translation =
        typeExpr
        |> TranslateTypeExprFromTypes.translateTypeExprFromTypes ~config
             ~typeEnv
      in
      translation |> handleGeneralDeclaration |> returnTypeDeclaration
  | GeneralDeclaration (Some coreType), None ->
      let translation =
        coreType |> TranslateCoreType.translateCoreType ~config ~typeEnv
      in
      let type_ =
        match (coreType, translation.type_) with
        | { ctyp_desc = Ttyp_variant (rowFields, _, _) }, Variant variant ->
            let rowFieldsVariants =
              rowFields |> TranslateCoreType.processVariant
            in
            let noPayloads =
              rowFieldsVariants.noPayloads |> List.map createCase
            in
            let payloads =
              if
                variant.payloads |> List.length
                = (rowFieldsVariants.payloads |> List.length)
              then
                List.combine variant.payloads rowFieldsVariants.payloads
                |> List.map (fun (payload, (label, attributes, _)) ->
                       let case = (label, attributes) |> createCase in
                       { payload with case })
              else variant.payloads
            in
            createVariant ~bsStringOrInt:false ~inherits:variant.inherits
              ~noPayloads ~payloads ~polymorphic:true
        | _ -> translation.type_
      in
      { translation with type_ } |> handleGeneralDeclaration
      |> returnTypeDeclaration
  | RecordDeclarationFromTypes labelDeclarations, None ->
      let { TranslateTypeExprFromTypes.dependencies; type_ } =
        labelDeclarations |> translateLabelDeclarations
      in
      let importTypes =
        dependencies
        |> Translation.translateDependencies ~config ~outputFileRelative
             ~resolver
      in
      {
        CodeItem.importTypes;
        exportFromTypeDeclaration =
          typeName
          |> createExportTypeFromTypeDeclaration ~annotation ~loc ~nameAs
               ~opaque ~type_ ~typeEnv ~typeVars;
      }
      |> returnTypeDeclaration
  | VariantDeclarationFromTypes constructorDeclarations, None ->
      let recordGen = Runtime.recordGen () in
      let variants =
        constructorDeclarations
        |> List.map (fun constructorDeclaration ->
               let constructorArgs = constructorDeclaration.Types.cd_args in
               let name = constructorDeclaration.Types.cd_id |> Ident.name in
               let attributes = constructorDeclaration.Types.cd_attributes in
               let argsTranslation =
                 match constructorArgs with
                 | Cstr_tuple typeExprs ->
                     typeExprs
                     |> TranslateTypeExprFromTypes.translateTypeExprsFromTypes
                          ~config ~typeEnv
                 | Cstr_record labelDeclarations ->
                     [ labelDeclarations |> translateLabelDeclarations ]
               in
               let inlineRecord =
                 match constructorArgs with
                 | Cstr_tuple _ -> false
                 | Cstr_record _ -> true
               in
               let argTypes =
                 argsTranslation
                 |> List.map (fun { TranslateTypeExprFromTypes.type_ } -> type_)
               in
               let importTypes =
                 argsTranslation
                 |> List.map (fun { TranslateTypeExprFromTypes.dependencies } ->
                        dependencies)
                 |> List.concat
                 |> Translation.translateDependencies ~config
                      ~outputFileRelative ~resolver
               in
               let recordValue =
                 recordGen
                 |> Runtime.newRecordValue
                      ~unboxed:(constructorArgs = Cstr_tuple [])
               in
               ( name,
                 attributes,
                 argTypes,
                 importTypes,
                 inlineRecord,
                 recordValue |> Runtime.recordValueToString ))
      in
      let variantsNoPayload, variantsWithPayload =
        variants
        |> List.partition (fun (_, _, argTypes, _, _, _) -> argTypes = [])
      in
      let noPayloads =
        variantsNoPayload
        |> List.map
             (fun
               ( name,
                 attributes,
                 _argTypes,
                 _importTypes,
                 _inlineRecord,
                 recordValue )
             -> { ((name, attributes) |> createCase) with label = recordValue })
      in
      let payloads =
        variantsWithPayload
        |> List.map
             (fun
               ( name,
                 attributes,
                 argTypes,
                 _importTypes,
                 inlineRecord,
                 recordValue )
             ->
               let type_ =
                 match argTypes with [ type_ ] -> type_ | _ -> Tuple argTypes
               in
               let numArgs = argTypes |> List.length in
               {
                 case =
                   {
                     ((name, attributes) |> createCase) with
                     label = recordValue;
                   };
                 inlineRecord;
                 numArgs;
                 t = type_;
               })
      in
      let variantTyp =
        match (noPayloads, payloads) with
        | [], [ { t = type_ } ] when unboxedAnnotation -> type_
        | _ ->
            createVariant ~bsStringOrInt:false ~inherits:[] ~noPayloads
              ~payloads ~polymorphic:false
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
            };
          annotation;
        }
      in
      let importTypes =
        variants
        |> List.map (fun (_, _, _, importTypes, _, _) -> importTypes)
        |> List.concat
      in
      { CodeItem.exportFromTypeDeclaration; importTypes }
      |> returnTypeDeclaration
  | NoDeclaration, None -> []

let hasSomeGADTLeaf constructorDeclarations =
  List.exists
    (fun declaration -> declaration.Types.cd_res != None)
    constructorDeclarations

let translateTypeDeclaration ~config ~outputFileRelative ~recursive ~resolver
    ~typeEnv
    ({ typ_attributes; typ_id; typ_loc; typ_manifest; typ_params; typ_type } :
      Typedtree.type_declaration) : CodeItem.typeDeclaration list =
  if !Debug.translation then
    Log_.item "Translate Type Declaration %s\n" (typ_id |> Ident.name);
  if recursive then typeEnv |> TypeEnv.newType ~name:(typ_id |> Ident.name);
  let typeName = Ident.name typ_id in
  let typeVars =
    typ_params
    |> List.map (fun (coreType, _) -> coreType)
    |> TypeVars.extractFromCoreType
  in
  let declarationKind =
    match typ_type.type_kind with
    | Type_record (labelDeclarations, _) ->
        RecordDeclarationFromTypes labelDeclarations
    | Type_variant constructorDeclarations ->
        VariantDeclarationFromTypes constructorDeclarations
    | Type_abstract -> GeneralDeclaration typ_manifest
    | _ -> NoDeclaration
  in
  let res =
    declarationKind
    |> traslateDeclarationKind ~config ~loc:typ_loc ~outputFileRelative
         ~resolver ~typeAttributes:typ_attributes ~typeEnv ~typeName ~typeVars
  in
  if not recursive then typeEnv |> TypeEnv.newType ~name:(typ_id |> Ident.name);
  res

let translateTypeDeclarations ~config ~outputFileRelative ~recursive ~resolver
    ~typeEnv (typeDeclarations : Typedtree.type_declaration list) :
    CodeItem.typeDeclaration list =
  typeDeclarations
  |> List.map
       (translateTypeDeclaration ~config ~outputFileRelative ~recursive
          ~resolver ~typeEnv)
  |> List.concat
