open GenTypeCommon
open! TranslateTypeExprFromTypes

let removeOption ~(label : Asttypes.arg_label) (coreType : Typedtree.core_type)
    =
  match (coreType.ctyp_desc, label) with
  | Ttyp_constr (Path.Pident id, _, [ t ]), Optional lbl
    when Ident.name id = "option" ->
      Some (lbl, t)
  | Ttyp_constr (Pdot (Path.Pident nameSpace, id, _), _, [ t ]), Optional lbl
    when (* This has a different representation in 4.03+  *)
         Ident.name nameSpace = "FB" && id = "option" ->
      Some (lbl, t)
  | _ -> None

type processVariant = {
  noPayloads : (string * Typedtree.attributes) list;
  payloads : (string * Typedtree.attributes * Typedtree.core_type) list;
  inherits : Typedtree.core_type list;
}

let processVariant rowFields =
  let rec loop ~noPayloads ~payloads ~inherits fields =
    match fields with
    | Typedtree.Ttag
        ({ txt = label }, attributes, _, (* only variants with no payload *) [])
      :: otherFields ->
        otherFields
        |> loop
             ~noPayloads:((label, attributes) :: noPayloads)
             ~payloads ~inherits
    | Ttag ({ txt = label }, attributes, _, [ payload ]) :: otherFields ->
        otherFields
        |> loop ~noPayloads
             ~payloads:((label, attributes, payload) :: payloads)
             ~inherits
    | Ttag (_, _, _, _ :: _ :: _) :: otherFields ->
        (* Unknown: skipping *)
        otherFields |> loop ~noPayloads ~payloads ~inherits
    | Tinherit t :: otherFields ->
        otherFields |> loop ~noPayloads ~payloads ~inherits:(t :: inherits)
    | [] ->
        {
          noPayloads = noPayloads |> List.rev;
          payloads = payloads |> List.rev;
          inherits = inherits |> List.rev;
        }
  in
  rowFields |> loop ~noPayloads:[] ~payloads:[] ~inherits:[]

let rec translateArrowType ~config ~typeVarsGen ~noFunctionReturnDependencies
    ~typeEnv ~revArgDeps ~revArgs (coreType : Typedtree.core_type) =
  match coreType.ctyp_desc with
  | Ttyp_arrow (Nolabel, coreType1, coreType2) ->
      let { dependencies; type_ } =
        coreType1 |> fun __x ->
        translateCoreType_ ~config ~typeVarsGen ~typeEnv __x
      in
      let nextRevDeps = List.rev_append dependencies revArgDeps in
      coreType2
      |> translateArrowType ~config ~typeVarsGen ~noFunctionReturnDependencies
           ~typeEnv ~revArgDeps:nextRevDeps
           ~revArgs:((Nolabel, type_) :: revArgs)
  | Ttyp_arrow (((Labelled lbl | Optional lbl) as label), coreType1, coreType2)
    -> (
      let asLabel =
        match coreType.ctyp_attributes |> Annotation.getGenTypeAsRenaming with
        | Some s -> s
        | None -> ""
      in
      match coreType1 |> removeOption ~label with
      | None ->
          let { dependencies; type_ = type1 } =
            coreType1 |> translateCoreType_ ~config ~typeVarsGen ~typeEnv
          in
          let nextRevDeps = List.rev_append dependencies revArgDeps in
          coreType2
          |> translateArrowType ~config ~typeVarsGen
               ~noFunctionReturnDependencies ~typeEnv ~revArgDeps:nextRevDeps
               ~revArgs:
                 (( Label
                      (match asLabel = "" with
                      | true -> lbl |> Runtime.mangleObjectField
                      | false -> asLabel),
                    type1 )
                 :: revArgs)
      | Some (lbl, t1) ->
          let { dependencies; type_ = type1 } =
            t1 |> translateCoreType_ ~config ~typeVarsGen ~typeEnv
          in
          let nextRevDeps = List.rev_append dependencies revArgDeps in
          coreType2
          |> translateArrowType ~config ~typeVarsGen
               ~noFunctionReturnDependencies ~typeEnv ~revArgDeps:nextRevDeps
               ~revArgs:
                 (( OptLabel
                      (match asLabel = "" with
                      | true -> lbl |> Runtime.mangleObjectField
                      | false -> asLabel),
                    type1 )
                 :: revArgs))
  | _ ->
      let { dependencies; type_ = retType } =
        coreType |> translateCoreType_ ~config ~typeVarsGen ~typeEnv
      in
      let allDeps =
        List.rev_append revArgDeps
          (match noFunctionReturnDependencies with
          | true -> []
          | false -> dependencies)
      in
      let labeledConvertableTypes = revArgs |> List.rev in
      let argTypes = labeledConvertableTypes |> NamedArgs.group in
      let functionType =
        Function
          {
            argTypes;
            componentName = None;
            retType;
            typeVars = [];
            uncurried = false;
          }
      in
      { dependencies = allDeps; type_ = functionType }

and translateCoreType_ ~config ~typeVarsGen
    ?(noFunctionReturnDependencies = false) ~typeEnv
    (coreType : Typedtree.core_type) =
  match coreType.ctyp_desc with
  | Ttyp_alias (ct, _) ->
      ct
      |> translateCoreType_ ~config ~typeVarsGen
           ~noFunctionReturnDependencies:false ~typeEnv
  | Ttyp_object (tObj, closedFlag) ->
      let getFieldType objectField =
        match objectField with
        | Typedtree.OTtag ({ txt = name }, _, t) -> (
            ( name,
              match name |> Runtime.isMutableObjectField with
              | true -> { dependencies = []; type_ = ident "" }
              | false -> t |> translateCoreType_ ~config ~typeVarsGen ~typeEnv
            ))
        | OTinherit t ->
            ("Inherit", t |> translateCoreType_ ~config ~typeVarsGen ~typeEnv)
      in
      let fieldsTranslations = tObj |> List.map getFieldType in
      translateObjType
        (match closedFlag = Closed with true -> Closed | false -> Open)
        fieldsTranslations
  | Ttyp_constr (path, _, typeParams) ->
      let paramsTranslation =
        typeParams |> translateCoreTypes_ ~config ~typeVarsGen ~typeEnv
      in
      TranslateTypeExprFromTypes.translateConstr ~config ~paramsTranslation
        ~path ~typeEnv
  | Ttyp_poly (_, t) ->
      t
      |> translateCoreType_ ~config ~typeVarsGen ~noFunctionReturnDependencies
           ~typeEnv
  | Ttyp_arrow _ ->
      coreType
      |> translateArrowType ~config ~typeVarsGen ~noFunctionReturnDependencies
           ~typeEnv ~revArgDeps:[] ~revArgs:[]
  | Ttyp_tuple listExp ->
      let innerTypesTranslation =
        listExp |> translateCoreTypes_ ~config ~typeVarsGen ~typeEnv
      in
      let innerTypes =
        innerTypesTranslation |> List.map (fun { type_ } -> type_)
      in
      let innerTypesDeps =
        innerTypesTranslation
        |> List.map (fun { dependencies } -> dependencies)
        |> List.concat
      in
      let tupleType = Tuple innerTypes in
      { dependencies = innerTypesDeps; type_ = tupleType }
  | Ttyp_var s -> { dependencies = []; type_ = TypeVar s }
  | Ttyp_variant (rowFields, _, _) -> (
      match rowFields |> processVariant with
      | { noPayloads; payloads; inherits } ->
          let bsString =
            coreType.ctyp_attributes
            |> Annotation.hasAttribute Annotation.tagIsBsString
          in
          let bsInt =
            coreType.ctyp_attributes
            |> Annotation.hasAttribute Annotation.tagIsBsInt
          in
          let lastBsInt = ref (-1) in
          let noPayloads =
            noPayloads
            |> List.map (fun (label, attributes) ->
                   let labelJS =
                     if bsString then
                       match attributes |> Annotation.getBsAsRenaming with
                       | Some labelRenamed -> StringLabel labelRenamed
                       | None -> StringLabel label
                     else if bsInt then (
                       match attributes |> Annotation.getBsAsInt with
                       | Some n ->
                           lastBsInt := n;
                           IntLabel (string_of_int n)
                       | None ->
                           lastBsInt := !lastBsInt + 1;
                           IntLabel (string_of_int !lastBsInt))
                     else StringLabel label
                   in
                   { label; labelJS })
          in
          let payloadsTranslations =
            payloads
            |> List.map (fun (label, attributes, payload) ->
                   ( label,
                     attributes,
                     payload |> translateCoreType_ ~config ~typeVarsGen ~typeEnv
                   ))
          in
          let payloads =
            payloadsTranslations
            |> List.map (fun (label, _attributes, translation) ->
                   {
                     case = { label; labelJS = StringLabel label };
                     inlineRecord = false;
                     numArgs = 1;
                     t = translation.type_;
                   })
          in
          let inheritsTranslations =
            inherits |> translateCoreTypes_ ~config ~typeVarsGen ~typeEnv
          in
          let inherits =
            inheritsTranslations |> List.map (fun { type_ } -> type_)
          in
          let type_ =
            createVariant ~bsStringOrInt:(bsString || bsInt) ~noPayloads
              ~payloads ~inherits ~polymorphic:true
          in
          let dependencies =
            (inheritsTranslations
            |> List.map (fun { dependencies } -> dependencies)
            |> List.concat)
            @ (payloadsTranslations
              |> List.map (fun (_, _, { dependencies }) -> dependencies)
              |> List.concat)
          in
          { dependencies; type_ })
  | Ttyp_package { pack_path; pack_fields } -> (
      match typeEnv |> TypeEnv.lookupModuleTypeSignature ~path:pack_path with
      | Some (signature, typeEnv) ->
          let typeEquationsTranslation =
            pack_fields
            |> List.map (fun (x, t) ->
                   ( x.Asttypes.txt,
                     t |> translateCoreType_ ~config ~typeVarsGen ~typeEnv ))
          in
          let typeEquations =
            typeEquationsTranslation
            |> List.map (fun (x, translation) -> (x, translation.type_))
          in
          let dependenciesFromTypeEquations =
            typeEquationsTranslation
            |> List.map (fun (_, translation) -> translation.dependencies)
            |> List.flatten
          in
          let typeEnv1 = typeEnv |> TypeEnv.addTypeEquations ~typeEquations in
          let dependenciesFromRecordType, type_ =
            signature.sig_type
            |> signatureToModuleRuntimeRepresentation ~config ~typeVarsGen
                 ~typeEnv:typeEnv1
          in
          {
            dependencies =
              dependenciesFromTypeEquations @ dependenciesFromRecordType;
            type_;
          }
      | None -> { dependencies = []; type_ = unknown })
  | Ttyp_any | Ttyp_class _ -> { dependencies = []; type_ = unknown }

and translateCoreTypes_ ~config ~typeVarsGen ~typeEnv typeExprs :
    translation list =
  typeExprs |> List.map (translateCoreType_ ~config ~typeVarsGen ~typeEnv)

let translateCoreType ~config ~typeEnv coreType =
  let typeVarsGen = GenIdent.createTypeVarsGen () in
  let translation =
    coreType |> translateCoreType_ ~config ~typeVarsGen ~typeEnv
  in
  if !Debug.dependencies then
    translation.dependencies
    |> List.iter (fun dep -> Log_.item "Dependency: %s\n" (dep |> depToString));
  translation
