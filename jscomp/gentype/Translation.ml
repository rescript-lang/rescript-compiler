open GenTypeCommon

type t = CodeItem.translation

let empty = ({ importTypes = []; codeItems = []; typeDeclarations = [] } : t)

let getImportTypeUniqueName ({ typeName; asTypeName } : CodeItem.importType) =
  typeName ^ match asTypeName with None -> "" | Some s -> "_as_" ^ s

let importTypeCompare i1 i2 =
  compare (i1 |> getImportTypeUniqueName) (i2 |> getImportTypeUniqueName)

let combine (translations : t list) : t =
  ( translations
  |> List.map (fun { CodeItem.importTypes; codeItems; typeDeclarations } ->
         ((importTypes, codeItems), typeDeclarations))
  |> List.split
  |> fun (x, y) -> (x |> List.split, y) )
  |> fun ((importTypes, codeItems), typeDeclarations) ->
  {
    CodeItem.importTypes = importTypes |> List.concat;
    codeItems = codeItems |> List.concat;
    typeDeclarations = typeDeclarations |> List.concat;
  }

(** Applies type parameters to types (for all) *)
let abstractTheTypeParameters ~typeVars type_ =
  match type_ with
  | Function function_ -> Function { function_ with typeVars }
  | _ -> type_

let depToImportType ~config ~outputFileRelative ~resolver (dep : dep) =
  match dep with
  | _ when dep |> Dependencies.isInternal -> []
  | External name when name = "list" ->
      [
        {
          CodeItem.typeName = "list";
          asTypeName = None;
          importPath =
            ModuleName.rescriptPervasives
            |> ModuleResolver.importPathForReasonModuleName ~config
                 ~outputFileRelative ~resolver;
        };
      ]
  | External _ -> []
  | Internal _ -> []
  | Dot _ ->
      let moduleName = dep |> Dependencies.getOuterModuleName in
      let typeName =
        dep |> Dependencies.removeExternalOuterModule |> depToString
      in
      let asTypeName =
        match dep |> Dependencies.isInternal with
        | true -> None
        | false -> Some (dep |> depToString)
      in
      let importPath =
        moduleName
        |> ModuleResolver.importPathForReasonModuleName ~config
             ~outputFileRelative ~resolver
      in
      [ { typeName; asTypeName; importPath } ]

let translateDependencies ~config ~outputFileRelative ~resolver dependencies :
    CodeItem.importType list =
  dependencies
  |> List.map (depToImportType ~config ~outputFileRelative ~resolver)
  |> List.concat

let translateValue ~attributes ~config ~docString ~outputFileRelative ~resolver
    ~typeEnv ~typeExpr ~(addAnnotationsToFunction : type_ -> type_) name : t =
  let nameAs =
    match Annotation.getGenTypeAsRenaming attributes with
    | Some s -> s
    | _ -> name
  in
  let typeExprTranslation =
    typeExpr
    |> TranslateTypeExprFromTypes.translateTypeExprFromTypes ~config ~typeEnv
  in
  let typeVars = typeExprTranslation.type_ |> TypeVars.free in
  let type_ =
    typeExprTranslation.type_
    |> abstractTheTypeParameters ~typeVars
    |> addAnnotationsToFunction
  in
  let resolvedNameOriginal =
    name |> TypeEnv.addModulePath ~typeEnv |> ResolvedName.toString
  in
  let resolvedName = nameAs |> TypeEnv.addModulePath ~typeEnv in
  let moduleAccessPath =
    typeEnv |> TypeEnv.getModuleAccessPath ~name:resolvedNameOriginal
  in
  let codeItems =
    [
      CodeItem.ExportValue
        {
          docString;
          moduleAccessPath;
          originalName = name;
          resolvedName;
          type_;
        };
    ]
  in
  {
    importTypes =
      typeExprTranslation.dependencies
      |> translateDependencies ~config ~outputFileRelative ~resolver;
    codeItems;
    typeDeclarations = [];
  }

(**
 [@genType]
 [@bs.module] external myBanner : ReasonReact.reactClass = "./MyBanner";
*)
let translatePrimitive ~config ~outputFileRelative ~resolver ~typeEnv
    (valueDescription : Typedtree.value_description) : t =
  if !Debug.translation then Log_.item "Translate Primitive\n";
  let valueName =
    match valueDescription.val_prim with
    | "" :: _ | [] -> valueDescription.val_id |> Ident.name
    | nameOfExtern :: _ ->
        (* extern foo : someType = "abc"
           The first element of val_prim is "abc" *)
        nameOfExtern
  in
  let typeExprTranslation =
    valueDescription.val_desc
    |> TranslateCoreType.translateCoreType ~config ~typeEnv
  in
  let attributeImport, attributeRenaming =
    valueDescription.val_attributes |> Annotation.getAttributeImportRenaming
  in
  match (typeExprTranslation.type_, attributeImport) with
  | _, Some importString ->
      let asPath =
        match attributeRenaming with Some asPath -> asPath | None -> valueName
      in
      let typeVars = typeExprTranslation.type_ |> TypeVars.free in
      let type_ =
        typeExprTranslation.type_ |> abstractTheTypeParameters ~typeVars
      in
      {
        importTypes =
          typeExprTranslation.dependencies
          |> translateDependencies ~config ~outputFileRelative ~resolver;
        codeItems =
          [
            ImportValue
              {
                asPath;
                importAnnotation = importString |> Annotation.importFromString;
                type_;
                valueName;
              };
          ];
        typeDeclarations = [];
      }
  | _ -> { importTypes = []; codeItems = []; typeDeclarations = [] }

let addTypeDeclarationsFromModuleEquations ~typeEnv (translation : t) =
  let eqs = typeEnv |> TypeEnv.getModuleEquations in
  let newTypeDeclarations =
    translation.typeDeclarations
    |> List.map (fun (typeDeclaration : CodeItem.typeDeclaration) ->
           let exportType =
             typeDeclaration.exportFromTypeDeclaration.exportType
           in
           let equations =
             exportType.resolvedTypeName |> ResolvedName.applyEquations ~eqs
           in
           equations
           |> List.map (fun (x, y) ->
                  let newExportType =
                    {
                      exportType with
                      nameAs = None;
                      type_ =
                        y |> ResolvedName.toString
                        |> ident ~builtin:false
                             ~typeArgs:
                               (exportType.typeVars
                               |> List.map (fun s -> TypeVar s));
                      resolvedTypeName = x;
                    }
                  in
                  {
                    CodeItem.exportFromTypeDeclaration =
                      {
                        CodeItem.exportType = newExportType;
                        annotation =
                          typeDeclaration.exportFromTypeDeclaration.annotation;
                      };
                    importTypes = [];
                  }))
    |> List.concat
  in
  match newTypeDeclarations = [] with
  | true -> translation
  | false ->
      {
        translation with
        typeDeclarations = translation.typeDeclarations @ newTypeDeclarations;
      }
