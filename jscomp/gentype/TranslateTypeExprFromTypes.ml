open GenTypeCommon

type translation = { dependencies : dep list; type_ : type_ }

let rec removeOption ~(label : Asttypes.arg_label) (typeExpr : Types.type_expr)
    =
  match (typeExpr.desc, label) with
  | Tconstr (Path.Pident id, [ t ], _), Optional lbl
    when Ident.name id = "option" ->
      Some (lbl, t)
  | Tconstr (Pdot (Path.Pident nameSpace, id, _), [ t ], _), Optional lbl
    when Ident.name nameSpace = "FB" && id = "option" ->
      Some (lbl, t)
  | Tlink t, _ -> t |> removeOption ~label
  | _ -> None

let rec pathToList path =
  match path with
  | Path.Pident id -> [ id |> Ident.name ]
  | Path.Pdot (p, s, _) -> s :: (p |> pathToList)
  | Path.Papply _ -> []

let translateObjType closedFlag fieldsTranslations =
  let dependencies =
    fieldsTranslations
    |> List.map (fun (_, { dependencies }) -> dependencies)
    |> List.concat
  in
  let rec checkMutableField ?(acc = []) fields =
    match fields with
    | (previousName, { type_ = _ }) :: (name, { type_ }) :: rest
      when Runtime.checkMutableObjectField ~previousName ~name ->
        (* The field was annotated "@bs.set" *)
        rest |> checkMutableField ~acc:((name, type_, Mutable) :: acc)
    | (name, { type_ }) :: rest ->
        rest |> checkMutableField ~acc:((name, type_, Immutable) :: acc)
    | [] -> acc |> List.rev
  in
  let fields =
    fieldsTranslations |> checkMutableField
    |> List.map (fun (name, t, mutable_) ->
           let optional, type_ =
             match t with Option t -> (Optional, t) | _ -> (Mandatory, t)
           in
           let name = name |> Runtime.mangleObjectField in
           { mutable_; nameJS = name; nameRE = name; optional; type_ })
  in
  let type_ = Object (closedFlag, fields) in
  { dependencies; type_ }

let translateConstr ~config ~paramsTranslation ~(path : Path.t) ~typeEnv =
  let defaultCase () =
    let typeArgs =
      paramsTranslation |> List.map (fun ({ type_ } : translation) -> type_)
    in
    let typeParamDeps =
      paramsTranslation
      |> List.map (fun { dependencies } -> dependencies)
      |> List.concat
    in
    match typeEnv |> TypeEnv.applyTypeEquations ~config ~path with
    | Some type_ -> { dependencies = typeParamDeps; type_ }
    | None ->
        let dep = path |> Dependencies.fromPath ~config ~typeEnv in
        {
          dependencies = dep :: typeParamDeps;
          type_ = Ident { builtin = false; name = dep |> depToString; typeArgs };
        }
  in
  match (path |> pathToList |> List.rev, paramsTranslation) with
  | ([ "FB"; "bool" ] | [ "bool" ]), [] ->
      { dependencies = []; type_ = booleanT }
  | ([ "FB"; "int" ] | [ "int" ]), [] -> { dependencies = []; type_ = numberT }
  | ([ "Int64"; "t" ] | [ "int64" ]), [] ->
      { dependencies = []; type_ = int64T }
  | ([ "FB"; "float" ] | [ "float" ]), [] ->
      { dependencies = []; type_ = numberT }
  | ( ( [ "FB"; "string" ]
      | [ "string" ]
      | [ "String"; "t" ]
      | [ "Js"; ("String" | "String2"); "t" ] ),
      [] ) ->
      { dependencies = []; type_ = stringT }
  | [ "Js"; "Date"; "t" ], [] -> { dependencies = []; type_ = dateT }
  | ([ "FB"; "unit" ] | [ "unit" ]), [] -> { dependencies = []; type_ = unitT }
  | ( ([ "FB"; "array" ] | [ "array" ] | [ "Js"; ("Array" | "Array2"); "t" ]),
      [ paramTranslation ] ) ->
      { paramTranslation with type_ = Array (paramTranslation.type_, Mutable) }
  | [ "ImmutableArray"; "t" ], [ paramTranslation ] ->
      {
        paramTranslation with
        type_ = Array (paramTranslation.type_, Immutable);
      }
  | [ "Pervasives"; "ref" ], [ paramTranslation ] ->
      {
        dependencies = paramTranslation.dependencies;
        type_ =
          Object
            ( Closed,
              [
                {
                  mutable_ = Mutable;
                  nameJS = "contents";
                  nameRE = "contents";
                  optional = Mandatory;
                  type_ = paramTranslation.type_;
                };
              ] );
      }
  | ( ([ "Pervasives"; "result" ] | [ "Belt"; "Result"; "t" ]),
      [ paramTranslation1; paramTranslation2 ] ) ->
      let case n name type_ =
        {
          case = { label = string_of_int n; labelJS = StringLabel name };
          inlineRecord = false;
          numArgs = 1;
          t = type_;
        }
      in
      let variant =
        createVariant ~bsStringOrInt:false ~inherits:[] ~noPayloads:[]
          ~payloads:
            [
              case 0 "Ok" paramTranslation1.type_;
              case 1 "Error" paramTranslation2.type_;
            ]
          ~polymorphic:false
      in
      {
        dependencies =
          paramTranslation1.dependencies @ paramTranslation2.dependencies;
        type_ = variant;
      }
  | [ "React"; "callback" ], [ fromTranslation; toTranslation ] ->
      {
        dependencies = fromTranslation.dependencies @ toTranslation.dependencies;
        type_ =
          Function
            {
              argTypes = [ { aName = ""; aType = fromTranslation.type_ } ];
              componentName = None;
              retType = toTranslation.type_;
              typeVars = [];
              uncurried = false;
            };
      }
  | [ "React"; "componentLike" ], [ propsTranslation; retTranslation ] ->
      {
        dependencies =
          propsTranslation.dependencies @ retTranslation.dependencies;
        type_ =
          Function
            {
              argTypes = [ { aName = ""; aType = propsTranslation.type_ } ];
              componentName = None;
              retType = retTranslation.type_;
              typeVars = [];
              uncurried = false;
            };
      }
  | [ "React"; "component" ], [ propsTranslation ] ->
      {
        dependencies = propsTranslation.dependencies;
        type_ =
          Function
            {
              argTypes = [ { aName = ""; aType = propsTranslation.type_ } ];
              componentName = None;
              retType = EmitType.typeReactElement;
              typeVars = [];
              uncurried = false;
            };
      }
  | [ "React"; "Context"; "t" ], [ paramTranslation ] ->
      {
        dependencies = paramTranslation.dependencies;
        type_ = EmitType.typeReactContext ~type_:paramTranslation.type_;
      }
  | ([ "React"; "Ref"; "t" ] | [ "React"; "ref" ]), [ paramTranslation ] ->
      {
        dependencies = paramTranslation.dependencies;
        type_ = EmitType.typeReactRef ~type_:paramTranslation.type_;
      }
  | ([ "ReactDOM"; "domRef" ] | [ "ReactDOM"; "Ref"; "t" ]), [] ->
      { dependencies = []; type_ = EmitType.typeReactDOMReDomRef }
  | [ "ReactDOM"; "Ref"; "currentDomRef" ], [] ->
      { dependencies = []; type_ = EmitType.typeAny }
  | [ "ReactDOMRe"; "domRef" ], [] ->
      { dependencies = []; type_ = EmitType.typeReactDOMReDomRef }
  | [ "ReactDOMRe"; "Ref"; "currentDomRef" ], [] ->
      { dependencies = []; type_ = EmitType.typeAny }
  | [ "ReactEvent"; "Mouse"; "t" ], [] ->
      { dependencies = []; type_ = EmitType.typeReactEventMouseT }
  | ([ "React"; "element" ] | [ "ReasonReact"; "reactElement" ]), [] ->
      { dependencies = []; type_ = EmitType.typeReactElement }
  | ([ "FB"; "option" ] | [ "option" ]), [ paramTranslation ] ->
      { paramTranslation with type_ = Option paramTranslation.type_ }
  | ([ "Js"; "Null"; "t" ] | [ "Js"; "null" ]), [ paramTranslation ] ->
      { paramTranslation with type_ = Null paramTranslation.type_ }
  | ( ( [ "Js"; "Nullable"; "t" ]
      | [ "Js"; "nullable" ]
      | [ "Js"; "Null_undefined"; "t" ]
      | [ "Js"; "null_undefined" ] ),
      [ paramTranslation ] ) ->
      { paramTranslation with type_ = Nullable paramTranslation.type_ }
  | [ "Js"; "Promise"; "t" ], [ paramTranslation ] ->
      { paramTranslation with type_ = Promise paramTranslation.type_ }
  | ( [ "Js"; "Internal"; "fn" ],
      [ { dependencies = argsDependencies; type_ = Tuple ts }; ret ] ) ->
      {
        dependencies = argsDependencies @ ret.dependencies;
        type_ =
          Function
            {
              argTypes =
                ts |> List.map (fun type_ -> { aName = ""; aType = type_ });
              componentName = None;
              retType = ret.type_;
              typeVars = [];
              uncurried = true;
            };
      }
  | ( [ "Js"; "Internal"; "fn" ],
      [
        {
          dependencies = argsDependencies;
          type_ = Variant { noPayloads = [ { label = "Arity_0" } ] };
        };
        ret;
      ] ) ->
      {
        dependencies = argsDependencies @ ret.dependencies;
        type_ =
          Function
            {
              argTypes = [];
              componentName = None;
              retType = ret.type_;
              typeVars = [];
              uncurried = true;
            };
      }
  | [ "Js"; "Fn"; "arity0" ], [ ret ] ->
      {
        dependencies = ret.dependencies;
        type_ =
          Function
            {
              argTypes = [];
              componentName = None;
              retType = ret.type_;
              typeVars = [];
              uncurried = true;
            };
      }
  | ( [
        ("Js" | "Js_OO");
        ("Fn" | "Meth");
        ( "arity1" | "arity2" | "arity3" | "arity4" | "arity5" | "arity6"
        | "arity7" | "arity8" | "arity9" | "arity10" | "arity11" | "arity12"
        | "arity13" | "arity14" | "arity15" | "arity16" | "arity17" | "arity18"
        | "arity19" | "arity20" | "arity21" | "arity22" );
      ],
      [ arg ] ) ->
      {
        dependencies = arg.dependencies;
        type_ =
          (match arg.type_ with
          | Function function_ -> Function { function_ with uncurried = true }
          | _ -> arg.type_);
      }
  | ( [ "Js"; "Internal"; "fn" ],
      [ { dependencies = argsDependencies; type_ = singleT }; ret ] ) ->
      let argTypes =
        (match singleT with
        | Variant { payloads = [ { t = Tuple argTypes } ] } -> argTypes
        | Variant { payloads = [ { t = type_ } ] } -> [ type_ ]
        | _ -> [ singleT ])
        |> List.map (fun type_ -> { aName = ""; aType = type_ })
      in
      {
        dependencies = argsDependencies @ ret.dependencies;
        type_ =
          Function
            {
              argTypes;
              componentName = None;
              retType = ret.type_;
              typeVars = [];
              uncurried = true;
            };
      }
  | ( ([ "Js"; "Internal"; "meth" ] | [ "Js_internalOO"; "meth" ]),
      [
        {
          dependencies = argsDependencies;
          type_ =
            Variant
              { payloads = [ { case = { label = "Arity_1" }; t = type_ } ] };
        };
        ret;
      ] ) ->
      {
        dependencies = argsDependencies @ ret.dependencies;
        type_ =
          Function
            {
              argTypes = [ { aName = ""; aType = type_ } ];
              componentName = None;
              retType = ret.type_;
              typeVars = [];
              uncurried = true;
            };
      }
  | ( ([ "Js"; "Internal"; "meth" ] | [ "Js_internalOO"; "meth" ]),
      [
        {
          dependencies = argsDependencies;
          type_ = Variant { payloads = [ { t = Tuple ts } ] };
        };
        ret;
      ] ) ->
      {
        dependencies = argsDependencies @ ret.dependencies;
        type_ =
          Function
            {
              argTypes =
                ts |> List.map (fun type_ -> { aName = ""; aType = type_ });
              componentName = None;
              retType = ret.type_;
              typeVars = [];
              uncurried = true;
            };
      }
  | _ -> defaultCase ()

type processVariant = {
  noPayloads : string list;
  payloads : (string * Types.type_expr) list;
  unknowns : string list;
}

let processVariant rowFields =
  let rec loop ~noPayloads ~payloads ~unknowns fields =
    match fields with
    | ( label,
        ( Types.Rpresent (* no payload *) None
        | Reither ((* constant constructor *) true, _, _, _) ) )
      :: otherFields ->
        otherFields
        |> loop ~noPayloads:(label :: noPayloads) ~payloads ~unknowns
    | (label, Rpresent (Some payload)) :: otherFields ->
        otherFields
        |> loop ~noPayloads ~payloads:((label, payload) :: payloads) ~unknowns
    | (label, (Rabsent | Reither (false, _, _, _))) :: otherFields ->
        otherFields |> loop ~noPayloads ~payloads ~unknowns:(label :: unknowns)
    | [] ->
        {
          noPayloads = noPayloads |> List.rev;
          payloads = payloads |> List.rev;
          unknowns = unknowns |> List.rev;
        }
  in
  rowFields |> loop ~noPayloads:[] ~payloads:[] ~unknowns:[]

let rec translateArrowType ~config ~typeVarsGen ~typeEnv ~revArgDeps ~revArgs
    (typeExpr : Types.type_expr) =
  match typeExpr.desc with
  | Tlink t ->
      translateArrowType ~config ~typeVarsGen ~typeEnv ~revArgDeps ~revArgs t
  | Tarrow (Nolabel, typeExpr1, typeExpr2, _) ->
      let { dependencies; type_ } =
        typeExpr1 |> fun __x ->
        translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv __x
      in
      let nextRevDeps = List.rev_append dependencies revArgDeps in
      typeExpr2
      |> translateArrowType ~config ~typeVarsGen ~typeEnv
           ~revArgDeps:nextRevDeps
           ~revArgs:((Nolabel, type_) :: revArgs)
  | Tarrow (((Labelled lbl | Optional lbl) as label), typeExpr1, typeExpr2, _)
    -> (
      match typeExpr1 |> removeOption ~label with
      | None ->
          let { dependencies; type_ = type1 } =
            typeExpr1
            |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
          in
          let nextRevDeps = List.rev_append dependencies revArgDeps in
          typeExpr2
          |> translateArrowType ~config ~typeVarsGen ~typeEnv
               ~revArgDeps:nextRevDeps
               ~revArgs:
                 ((Label (lbl |> Runtime.mangleObjectField), type1) :: revArgs)
      | Some (lbl, t1) ->
          let { dependencies; type_ = type1 } =
            t1 |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
          in
          let nextRevDeps = List.rev_append dependencies revArgDeps in
          typeExpr2
          |> translateArrowType ~config ~typeVarsGen ~typeEnv
               ~revArgDeps:nextRevDeps
               ~revArgs:
                 ((OptLabel (lbl |> Runtime.mangleObjectField), type1)
                 :: revArgs))
  | _ ->
      let { dependencies; type_ = retType } =
        typeExpr |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
      in
      let allDeps = List.rev_append revArgDeps dependencies in
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

and translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
    (typeExpr : Types.type_expr) =
  match typeExpr.desc with
  | Tvar None ->
      let typeName =
        GenIdent.jsTypeNameForAnonymousTypeID ~typeVarsGen typeExpr.id
      in
      { dependencies = []; type_ = TypeVar typeName }
  | Tvar (Some s) -> { dependencies = []; type_ = TypeVar s }
  | Tconstr
      ( Pdot (Pident { name = "Js" }, "t", _),
        [ { desc = Tvar _ | Tconstr _ } ],
        _ ) ->
      (* Preserve some existing uses of Js.t(Obj.t) and Js.t('a). *)
      translateObjType Closed []
  | Tconstr (Pdot (Pident { name = "Js" }, "t", _), [ t ], _) ->
      t |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
  | Tobject (tObj, _) ->
      let rec getFieldTypes (texp : Types.type_expr) =
        match texp.desc with
        | Tfield (name, _, t1, t2) ->
            let closedFlafg, fields = t2 |> getFieldTypes in
            ( closedFlafg,
              ( name,
                match name |> Runtime.isMutableObjectField with
                | true -> { dependencies = []; type_ = ident "" }
                | false ->
                    t1
                    |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
              )
              :: fields )
        | Tlink te -> te |> getFieldTypes
        | Tvar None -> (Open, [])
        | _ -> (Closed, [])
      in
      let closedFlag, fieldsTranslations = tObj |> getFieldTypes in
      translateObjType closedFlag fieldsTranslations
  | Tconstr (path, [ { desc = Tlink te } ], r) ->
      { typeExpr with desc = Types.Tconstr (path, [ te ], r) }
      |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
  | Tconstr (path, typeParams, _) ->
      let paramsTranslation =
        typeParams |> translateTypeExprsFromTypes_ ~config ~typeVarsGen ~typeEnv
      in
      translateConstr ~config ~paramsTranslation ~path ~typeEnv
  | Tpoly (t, []) ->
      t |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
  | Tarrow _ ->
      typeExpr
      |> translateArrowType ~config ~typeVarsGen ~typeEnv ~revArgDeps:[]
           ~revArgs:[]
  | Ttuple listExp ->
      let innerTypesTranslation =
        listExp |> translateTypeExprsFromTypes_ ~config ~typeVarsGen ~typeEnv
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
  | Tlink t -> t |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
  | Tvariant rowDesc -> (
      match rowDesc.row_fields |> processVariant with
      | { noPayloads; payloads = []; unknowns = [] } ->
          let noPayloads =
            noPayloads
            |> List.map (fun label -> { label; labelJS = StringLabel label })
          in
          let type_ =
            createVariant ~bsStringOrInt:false ~inherits:[] ~noPayloads
              ~payloads:[] ~polymorphic:true
          in
          { dependencies = []; type_ }
      | { noPayloads = []; payloads = [ (_label, t) ]; unknowns = [] } ->
          (* Handle bucklescript's "Arity_" encoding in first argument of Js.Internal.fn(_,_) for uncurried functions.
             Return the argument tuple. *)
          t |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
      | { noPayloads; payloads; unknowns = [] } ->
          let noPayloads =
            noPayloads
            |> List.map (fun label -> { label; labelJS = StringLabel label })
          in
          let payloadTranslations =
            payloads
            |> List.map (fun (label, payload) ->
                   ( label,
                     payload
                     |> translateTypeExprFromTypes_ ~config ~typeVarsGen
                          ~typeEnv ))
          in
          let payloads =
            payloadTranslations
            |> List.map (fun (label, translation) ->
                   {
                     case = { label; labelJS = StringLabel label };
                     inlineRecord = false;
                     numArgs = 1;
                     t = translation.type_;
                   })
          in
          let type_ =
            createVariant ~bsStringOrInt:false ~inherits:[] ~noPayloads
              ~payloads ~polymorphic:true
          in
          let dependencies =
            payloadTranslations
            |> List.map (fun (_, { dependencies }) -> dependencies)
            |> List.concat
          in
          { dependencies; type_ }
      | { unknowns = _ :: _ } -> { dependencies = []; type_ = unknown })
  | Tpackage (path, ids, types) -> (
      match typeEnv |> TypeEnv.lookupModuleTypeSignature ~path with
      | Some (signature, typeEnv) ->
          let typeEquationsTranslation =
            List.combine ids types
            |> List.map (fun (x, t) ->
                   ( x,
                     t
                     |> translateTypeExprFromTypes_ ~config ~typeVarsGen
                          ~typeEnv ))
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
  | Tfield _ | Tnil | Tpoly _ | Tsubst _ | Tunivar _ ->
      { dependencies = []; type_ = unknown }

and translateTypeExprsFromTypes_ ~config ~typeVarsGen ~typeEnv typeExprs :
    translation list =
  typeExprs
  |> List.map (translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv)

and signatureToModuleRuntimeRepresentation ~config ~typeVarsGen ~typeEnv
    signature =
  let dependenciesAndFields =
    signature
    |> List.map (fun signatureItem ->
           match signatureItem with
           | Types.Sig_value (_id, { val_kind = Val_prim _ }) -> ([], [])
           | Types.Sig_value (id, { val_type = typeExpr }) ->
               let { dependencies; type_ } =
                 typeExpr
                 |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
               in
               let field =
                 {
                   mutable_ = Immutable;
                   nameJS = id |> Ident.name;
                   nameRE = id |> Ident.name;
                   optional = Mandatory;
                   type_;
                 }
               in
               (dependencies, [ field ])
           | Types.Sig_module (id, moduleDeclaration, _recStatus) ->
               let typeEnv1 =
                 match
                   typeEnv |> TypeEnv.getModule ~name:(id |> Ident.name)
                 with
                 | Some typeEnv1 -> typeEnv1
                 | None -> typeEnv
               in
               let dependencies, type_ =
                 match moduleDeclaration.md_type with
                 | Mty_signature signature ->
                     signature
                     |> signatureToModuleRuntimeRepresentation ~config
                          ~typeVarsGen ~typeEnv:typeEnv1
                 | Mty_ident _ | Mty_functor _ | Mty_alias _ -> ([], unknown)
               in
               let field =
                 {
                   mutable_ = Immutable;
                   nameJS = id |> Ident.name;
                   nameRE = id |> Ident.name;
                   optional = Mandatory;
                   type_;
                 }
               in
               (dependencies, [ field ])
           | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
           | Types.Sig_class _ | Types.Sig_class_type _ ->
               ([], []))
  in
  let dependencies, fields =
    let dl, fl = dependenciesAndFields |> List.split in
    (dl |> List.concat, fl |> List.concat)
  in
  (dependencies, Object (Closed, fields))

let translateTypeExprFromTypes ~config ~typeEnv typeExpr =
  let typeVarsGen = GenIdent.createTypeVarsGen () in
  let translation =
    typeExpr |> translateTypeExprFromTypes_ ~config ~typeVarsGen ~typeEnv
  in
  if !Debug.dependencies then
    translation.dependencies
    |> List.iter (fun dep -> Log_.item "Dependency: %s\n" (dep |> depToString));
  translation

let translateTypeExprsFromTypes ~config ~typeEnv typeExprs =
  let typeVarsGen = GenIdent.createTypeVarsGen () in
  let translations =
    typeExprs |> translateTypeExprsFromTypes_ ~config ~typeVarsGen ~typeEnv
  in
  if !Debug.dependencies then
    translations
    |> List.iter (fun translation ->
           translation.dependencies
           |> List.iter (fun dep ->
                  Log_.item "Dependency: %s\n" (dep |> depToString)));
  translations
