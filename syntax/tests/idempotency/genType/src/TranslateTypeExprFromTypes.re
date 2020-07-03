open GenTypeCommon;

type translation = {
  dependencies: list(dep),
  type_,
};

let rec removeOption = (~label: Asttypes.arg_label, typeExpr: Types.type_expr) =>
  switch (typeExpr.desc, label) {
  | (Tconstr(Path.Pident(id), [t], _), Optional(lbl))
      when Ident.name(id) == "option" =>
    Some((lbl, t))
  | (Tconstr(Pdot(Path.Pident(nameSpace), id, _), [t], _), Optional(lbl))
      when Ident.name(nameSpace) == "FB" && id == "option" =>
    Some((lbl, t))
  | (Tlink(t), _) => t |> removeOption(~label)
  | _ => None
  };

let rec pathToList = path =>
  switch (path) {
  | Path.Pident(id) => [id |> Ident.name]
  | Path.Pdot(p, s, _) => [s, ...p |> pathToList]
  | Path.Papply(_) => []
  };

let translateConstr =
    (
      ~config,
      ~fieldsTranslations,
      ~closedFlag,
      ~paramsTranslation,
      ~path: Path.t,
      ~typeEnv,
    ) => {
  let defaultCase = () => {
    let typeArgs =
      paramsTranslation |> List.map(({type_}: translation) => type_);
    let typeParamDeps =
      paramsTranslation
      |> List.map(({dependencies}) => dependencies)
      |> List.concat;

    switch (typeEnv |> TypeEnv.applyTypeEquations(~config, ~path)) {
    | Some(type_) => {dependencies: typeParamDeps, type_}
    | None =>
      let dep = path |> Dependencies.fromPath(~config, ~typeEnv);
      {
        dependencies: [dep, ...typeParamDeps],
        type_: Ident({builtin: false, name: dep |> depToString, typeArgs}),
      };
    };
  };
  switch (path |> pathToList |> List.rev, paramsTranslation) {
  | (["FB", "bool"] | ["bool"], []) => {dependencies: [], type_: booleanT}

  | (["FB", "int"] | ["int"], []) => {dependencies: [], type_: numberT}

  | (["Int64", "t"] | ["int64"], []) => {dependencies: [], type_: int64T}

  | (["FB", "float"] | ["float"], []) => {dependencies: [], type_: numberT}

  | (
      ["FB", "string"] | ["string"] | ["String", "t"] |
      ["Js", "String" | "String2", "t"],
      [],
    ) => {
      dependencies: [],
      type_: stringT,
    }

  | (["Js", "Date", "t"], []) => {dependencies: [], type_: dateT}

  | (["FB", "unit"] | ["unit"], []) => {dependencies: [], type_: unitT}

  | (
      ["FB", "array"] | ["array"] | ["Js", "Array" | "Array2", "t"],
      [paramTranslation],
    ) => {
      ...paramTranslation,
      type_: Array(paramTranslation.type_, Mutable),
    }

  | (["ImmutableArray", "t"], [paramTranslation]) => {
      ...paramTranslation,
      type_: Array(paramTranslation.type_, Immutable),
    }

  | (["Pervasives", "ref"], [paramTranslation]) => {
      dependencies: paramTranslation.dependencies,
      type_: Tuple([paramTranslation.type_]),
    }

  | (
      ["Pervasives", "result"] | ["Belt", "Result", "t"],
      [paramTranslation1, paramTranslation2],
    ) =>
    let case = (n, name, type_) => (
      {label: string_of_int(n), labelJS: StringLabel(name)},
      1,
      type_,
    );
    let variant =
      createVariant(
        ~noPayloads=[],
        ~payloads=[
          case(0, "Ok", paramTranslation1.type_),
          case(1, "Error", paramTranslation2.type_),
        ],
        ~polymorphic=false,
      );
    {
      dependencies:
        paramTranslation1.dependencies @ paramTranslation2.dependencies,
      type_: variant,
    };

  | (["React", "callback"], [fromTranslation, toTranslation]) => {
      dependencies: fromTranslation.dependencies @ toTranslation.dependencies,
      type_:
        Function({
          argTypes: [{aName: "", aType: fromTranslation.type_}],
          componentName: None,
          retType: toTranslation.type_,
          typeVars: [],
          uncurried: false,
        }),
    }

  | (["React", "componentLike"], [propsTranslation, retTranslation]) => {
      dependencies:
        propsTranslation.dependencies @ retTranslation.dependencies,
      type_:
        Function({
          argTypes: [{aName: "", aType: propsTranslation.type_}],
          componentName: None,
          retType: retTranslation.type_,
          typeVars: [],
          uncurried: false,
        }),
    }

  | (["React", "component"], [propsTranslation]) => {
      dependencies: propsTranslation.dependencies,
      type_:
        Function({
          argTypes: [{aName: "", aType: propsTranslation.type_}],
          componentName: None,
          retType: EmitType.typeReactElement(~config),
          typeVars: [],
          uncurried: false,
        }),
    }

  | (["React", "Context", "t"], [paramTranslation]) => {
      dependencies: paramTranslation.dependencies,
      type_:
        EmitType.typeReactContext(~config, ~type_=paramTranslation.type_),
    }

  | (["React", "Ref", "t"], [paramTranslation]) => {
      dependencies: paramTranslation.dependencies,
      type_: EmitType.typeReactRef(~type_=paramTranslation.type_),
    }

  | (["ReactDOMRe", "domRef"], []) => {
      dependencies: [],
      type_: EmitType.typeReactDOMReDomRef(~config),
    }

  | (["ReactDOMRe", "Ref", "currentDomRef"], []) => {
      dependencies: [],
      type_: EmitType.typeAny(~config),
    }

  | (["React", "element"] | ["ReasonReact", "reactElement"], []) => {
      dependencies: [],
      type_: EmitType.typeReactElement(~config),
    }

  | (["FB", "option"] | ["option"], [paramTranslation]) => {
      ...paramTranslation,
      type_: Option(paramTranslation.type_),
    }

  | (["Js", "Null", "t"] | ["Js", "null"], [paramTranslation]) => {
      ...paramTranslation,
      type_: Null(paramTranslation.type_),
    }

  | (
      ["Js", "Nullable", "t"] | ["Js", "nullable"] |
      ["Js", "Null_undefined", "t"] |
      ["Js", "null_undefined"],
      [paramTranslation],
    ) => {
      ...paramTranslation,
      type_: Nullable(paramTranslation.type_),
    }
  | (["Js", "Promise", "t"], [paramTranslation]) => {
      ...paramTranslation,
      type_: Promise(paramTranslation.type_),
    }
  | (
      ["Js", "Internal", "fn"],
      [{dependencies: argsDependencies, type_: Tuple(ts)}, ret],
    ) => {
      dependencies: argsDependencies @ ret.dependencies,
      type_:
        Function({
          argTypes: ts |> List.map(type_ => {aName: "", aType: type_}),
          componentName: None,
          retType: ret.type_,
          typeVars: [],
          uncurried: true,
        }),
    }

  | (
      ["Js", "Internal", "fn"],
      [
        {
          dependencies: argsDependencies,
          type_: Variant({noPayloads: [{label: "Arity_0"}]}),
        },
        ret,
      ],
    ) => {
      dependencies: argsDependencies @ ret.dependencies,
      type_:
        Function({
          argTypes: [],
          componentName: None,
          retType: ret.type_,
          typeVars: [],
          uncurried: true,
        }),
    }
  | (["Js", "Fn", "arity0"], [ret]) => {
      dependencies: ret.dependencies,
      type_:
        Function({
          argTypes: [],
          componentName: None,
          retType: ret.type_,
          typeVars: [],
          uncurried: true,
        }),
    }
  | (
      [
        "Js" | "Js_OO",
        "Fn" | "Meth",
        "arity1" | "arity2" | "arity3" | "arity4" | "arity5" | "arity6" |
        "arity7" |
        "arity8" |
        "arity9" |
        "arity10" |
        "arity11" |
        "arity12" |
        "arity13" |
        "arity14" |
        "arity15" |
        "arity16" |
        "arity17" |
        "arity18" |
        "arity19" |
        "arity20" |
        "arity21" |
        "arity22",
      ],
      [arg],
    ) => {
      dependencies: arg.dependencies,
      type_:
        switch (arg.type_) {
        | Function(function_) => Function({...function_, uncurried: true})
        | _ => arg.type_
        },
    }
  | (
      ["Js", "Internal", "fn"],
      [{dependencies: argsDependencies, type_: singleT}, ret],
    ) =>
    let argTypes =
      (
        switch (singleT) {
        | Variant({payloads: [(_, _, Tuple(argTypes))]}) => argTypes
        | Variant({payloads: [(_, _, type_)]}) => [type_]
        | _ => [singleT]
        }
      )
      |> List.map(type_ => {aName: "", aType: type_});
    {
      dependencies: argsDependencies @ ret.dependencies,
      type_:
        Function({
          argTypes,
          componentName: None,
          retType: ret.type_,
          typeVars: [],
          uncurried: true,
        }),
    };
  | (
      ["Js", "Internal", "meth"] | ["Js_internalOO", "meth"],
      [
        {
          dependencies: argsDependencies,
          type_: Variant({payloads: [({label: "Arity_1"}, _, type_)]}),
        },
        ret,
      ],
    ) => {
      dependencies: argsDependencies @ ret.dependencies,
      type_:
        Function({
          argTypes: [{aName: "", aType: type_}],
          componentName: None,
          retType: ret.type_,
          typeVars: [],
          uncurried: true,
        }),
    }
  | (
      ["Js", "Internal", "meth"] | ["Js_internalOO", "meth"],
      [
        {
          dependencies: argsDependencies,
          type_: Variant({payloads: [(_, _, Tuple(ts))]}),
        },
        ret,
      ],
    ) => {
      dependencies: argsDependencies @ ret.dependencies,
      type_:
        Function({
          argTypes: ts |> List.map(type_ => {aName: "", aType: type_}),
          componentName: None,
          retType: ret.type_,
          typeVars: [],
          uncurried: true,
        }),
    }
  | (["Js", "t"], _) =>
    let dependencies =
      fieldsTranslations
      |> List.map(((_, {dependencies})) => dependencies)
      |> List.concat;
    let rec checkMutableField = (~acc=[], fields) =>
      switch (fields) {
      | [(previousName, {type_: _}), (name, {type_}), ...rest]
          when Runtime.checkMutableObjectField(~previousName, ~name) =>
        /* The field was annotated "@bs.set" */
        rest |> checkMutableField(~acc=[(name, type_, Mutable), ...acc])
      | [(name, {type_}), ...rest] =>
        rest |> checkMutableField(~acc=[(name, type_, Immutable), ...acc])
      | [] => acc |> List.rev
      };
    let fields =
      fieldsTranslations
      |> checkMutableField
      |> List.map(((name, t, mutable_)) => {
           let (optional, type_) =
             switch (t) {
             | Option(t) => (Optional, t)
             | _ => (Mandatory, t)
             };
           let name = name |> Runtime.mangleObjectField;
           {mutable_, nameJS: name, nameRE: name, optional, type_};
         });
    let type_ = Object(closedFlag, fields);
    {dependencies, type_};

  | _ => defaultCase()
  };
};

type processVariant = {
  noPayloads: list(string),
  payloads: list((string, Types.type_expr)),
  unknowns: list(string),
};

let processVariant = rowFields => {
  let rec loop = (~noPayloads, ~payloads, ~unknowns, fields) =>
    switch (fields) {
    | [
        (
          label,
          Types.Rpresent(/* no payload */ None) |
          Reither(/* constant constructor */ true, _, _, _),
        ),
        ...otherFields,
      ] =>
      otherFields
      |> loop(~noPayloads=[label, ...noPayloads], ~payloads, ~unknowns)
    | [(label, Rpresent(Some(payload))), ...otherFields] =>
      otherFields
      |> loop(
           ~noPayloads,
           ~payloads=[(label, payload), ...payloads],
           ~unknowns,
         )
    | [(label, Rabsent | Reither(false, _, _, _)), ...otherFields] =>
      otherFields
      |> loop(~noPayloads, ~payloads, ~unknowns=[label, ...unknowns])
    | [] => {
        noPayloads: noPayloads |> List.rev,
        payloads: payloads |> List.rev,
        unknowns: unknowns |> List.rev,
      }
    };
  rowFields |> loop(~noPayloads=[], ~payloads=[], ~unknowns=[]);
};

let rec translateArrowType =
        (
          ~config,
          ~typeVarsGen,
          ~noFunctionReturnDependencies=false,
          ~typeEnv,
          ~revArgDeps,
          ~revArgs,
          typeExpr: Types.type_expr,
        ) =>
  switch (typeExpr.desc) {
  | Tlink(t) =>
    translateArrowType(
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      ~typeEnv,
      ~revArgDeps,
      ~revArgs,
      t,
    )
  | Tarrow(Nolabel, typeExpr1, typeExpr2, _) =>
    let {dependencies, type_} =
      typeExpr1
      |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv, _);
    let nextRevDeps = List.rev_append(dependencies, revArgDeps);
    typeExpr2
    |> translateArrowType(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
         ~revArgDeps=nextRevDeps,
         ~revArgs=[(Nolabel, type_), ...revArgs],
       );
  | Tarrow((Labelled(lbl) | Optional(lbl)) as label, typeExpr1, typeExpr2, _) =>
    switch (typeExpr1 |> removeOption(~label)) {
    | None =>
      let {dependencies, type_: type1} =
        typeExpr1
        |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      typeExpr2
      |> translateArrowType(
           ~config,
           ~typeVarsGen,
           ~noFunctionReturnDependencies,
           ~typeEnv,
           ~revArgDeps=nextRevDeps,
           ~revArgs=[
             (Label(lbl |> Runtime.mangleObjectField), type1),
             ...revArgs,
           ],
         );
    | Some((lbl, t1)) =>
      let {dependencies, type_: type1} =
        t1 |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv);
      let nextRevDeps = List.rev_append(dependencies, revArgDeps);
      typeExpr2
      |> translateArrowType(
           ~config,
           ~typeVarsGen,
           ~noFunctionReturnDependencies,
           ~typeEnv,
           ~revArgDeps=nextRevDeps,
           ~revArgs=[
             (OptLabel(lbl |> Runtime.mangleObjectField), type1),
             ...revArgs,
           ],
         );
    }
  | _ =>
    let {dependencies, type_: retType} =
      typeExpr |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv);
    let allDeps =
      List.rev_append(
        revArgDeps,
        noFunctionReturnDependencies ? [] : dependencies,
      );

    let labeledConvertableTypes = revArgs |> List.rev;
    let argTypes = labeledConvertableTypes |> NamedArgs.group;

    let functionType =
      Function({
        argTypes,
        componentName: None,
        retType,
        typeVars: [],
        uncurried: false,
      });

    {dependencies: allDeps, type_: functionType};
  }
and translateTypeExprFromTypes_ =
    (
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies=false,
      ~typeEnv,
      typeExpr: Types.type_expr,
    ) =>
  switch (typeExpr.desc) {
  | Tvar(None) =>
    let typeName =
      GenIdent.jsTypeNameForAnonymousTypeID(~typeVarsGen, typeExpr.id);
    {dependencies: [], type_: TypeVar(typeName)};

  | Tvar(Some(s)) => {dependencies: [], type_: TypeVar(s)}

  | Tconstr(
      Pdot(Pident({name: "Js"}), "t", _) as path,
      [{desc: Tobject(tObj, _)}],
      _,
    ) =>
    let rec getFieldTypes = (texp: Types.type_expr) =>
      switch (texp.desc) {
      | Tfield(name, _, t1, t2) =>
        let (closedFlafg, fields) = t2 |> getFieldTypes;
        (
          closedFlafg,
          [
            (
              name,
              name |> Runtime.isMutableObjectField
                ? {dependencies: [], type_: ident("")}
                : t1
                  |> translateTypeExprFromTypes_(
                       ~config,
                       ~typeVarsGen,
                       ~typeEnv,
                     ),
            ),
            ...fields,
          ],
        );
      | Tlink(te) => te |> getFieldTypes
      | Tvar(None) => (Open, [])
      | _ => (Closed, [])
      };
    let (closedFlag, fieldsTranslations) = tObj |> getFieldTypes;
    translateConstr(
      ~config,
      ~fieldsTranslations,
      ~closedFlag,
      ~paramsTranslation=[],
      ~path,
      ~typeEnv,
    );

  | Tconstr(path, [{desc: Tlink(te)}], r) =>
    {...typeExpr, desc: Types.Tconstr(path, [te], r)}
    |> translateTypeExprFromTypes_(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies=false,
         ~typeEnv,
       )

  | Tconstr(path, typeParams, _) =>
    let paramsTranslation =
      typeParams
      |> translateTypeExprsFromTypes_(~config, ~typeVarsGen, ~typeEnv);
    translateConstr(
      ~config,
      ~fieldsTranslations=[],
      ~closedFlag=Closed,
      ~paramsTranslation,
      ~path,
      ~typeEnv,
    );

  | Tpoly(t, []) =>
    t
    |> translateTypeExprFromTypes_(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
       )

  | Tarrow(_) =>
    typeExpr
    |> translateArrowType(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
         ~revArgDeps=[],
         ~revArgs=[],
       )
  | Ttuple(listExp) =>
    let innerTypesTranslation =
      listExp |> translateTypeExprsFromTypes_(~config, ~typeVarsGen, ~typeEnv);
    let innerTypes = innerTypesTranslation |> List.map(({type_}) => type_);
    let innerTypesDeps =
      innerTypesTranslation
      |> List.map(({dependencies}) => dependencies)
      |> List.concat;

    let tupleType = Tuple(innerTypes);

    {dependencies: innerTypesDeps, type_: tupleType};

  | Tlink(t) =>
    t
    |> translateTypeExprFromTypes_(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies,
         ~typeEnv,
       )

  | Tvariant(rowDesc) =>
    switch (rowDesc.row_fields |> processVariant) {
    | {noPayloads, payloads: [], unknowns: []} =>
      let noPayloads =
        noPayloads |> List.map(label => {label, labelJS: StringLabel(label)});
      let type_ = createVariant(~noPayloads, ~payloads=[], ~polymorphic=true);
      {dependencies: [], type_};

    | {noPayloads: [], payloads: [(_label, t)], unknowns: []} =>
      /* Handle bucklescript's "Arity_" encoding in first argument of Js.Internal.fn(_,_) for uncurried functions.
         Return the argument tuple. */
      t
      |> translateTypeExprFromTypes_(
           ~config,
           ~typeVarsGen,
           ~noFunctionReturnDependencies,
           ~typeEnv,
         )

    | {noPayloads, payloads, unknowns: []} =>
      let noPayloads =
        noPayloads |> List.map(label => {label, labelJS: StringLabel(label)});
      let payloadTranslations =
        payloads
        |> List.map(((label, payload)) =>
             (
               label,
               payload
               |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv),
             )
           );
      let payloads =
        payloadTranslations
        |> List.map(((label, translation)) => {
             let numArgs = 1;
             (
               {label, labelJS: StringLabel(label)},
               numArgs,
               translation.type_,
             );
           });
      let type_ = createVariant(~noPayloads, ~payloads, ~polymorphic=true);
      let dependencies =
        payloadTranslations
        |> List.map(((_, {dependencies})) => dependencies)
        |> List.concat;
      {dependencies, type_};

    | {unknowns: [_, ..._]} => {
        dependencies: [],
        type_: mixedOrUnknown(~config),
      }
    }

  | Tpackage(path, ids, types) =>
    switch (typeEnv |> TypeEnv.lookupModuleTypeSignature(~path)) {
    | Some((signature, typeEnv)) =>
      let typeEquationsTranslation =
        List.combine(ids, types)
        |> List.map(((x, t)) =>
             (
               x,
               t
               |> translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv),
             )
           );
      let typeEquations =
        typeEquationsTranslation
        |> List.map(((x, translation)) => (x, translation.type_));
      let dependenciesFromTypeEquations =
        typeEquationsTranslation
        |> List.map(((_, translation)) => translation.dependencies)
        |> List.flatten;
      let typeEnv1 = typeEnv |> TypeEnv.addTypeEquations(~typeEquations);
      let (dependenciesFromRecordType, type_) =
        signature.sig_type
        |> signatureToModuleRuntimeRepresentation(
             ~config,
             ~typeVarsGen,
             ~typeEnv=typeEnv1,
           );
      {
        dependencies:
          dependenciesFromTypeEquations @ dependenciesFromRecordType,
        type_,
      };
    | None => {dependencies: [], type_: mixedOrUnknown(~config)}
    }

  | Tfield(_)
  | Tnil
  | Tobject(_)
  | Tpoly(_)
  | Tsubst(_)
  | Tunivar(_) => {dependencies: [], type_: mixedOrUnknown(~config)}
  }
and translateTypeExprsFromTypes_ =
    (~config, ~typeVarsGen, ~typeEnv, typeExprs): list(translation) =>
  typeExprs
  |> List.map(translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv))
and signatureToModuleRuntimeRepresentation =
    (~config, ~typeVarsGen, ~typeEnv, signature) => {
  let dependenciesAndFields =
    signature
    |> List.map(signatureItem =>
         switch (signatureItem) {
         | Types.Sig_value(_id, {val_kind: Val_prim(_)}) => ([], [])
         | Types.Sig_value(id, {val_type: typeExpr}) =>
           let {dependencies, type_} =
             typeExpr
             |> translateTypeExprFromTypes_(
                  ~config,
                  ~typeVarsGen,
                  ~noFunctionReturnDependencies=false,
                  ~typeEnv,
                );
           let field = {
             mutable_: Immutable,
             nameJS: id |> Ident.name,
             nameRE: id |> Ident.name,
             optional: Mandatory,
             type_,
           };
           (dependencies, [field]);

         | Types.Sig_module(id, moduleDeclaration, _recStatus) =>
           let typeEnv1 =
             switch (typeEnv |> TypeEnv.getModule(~name=id |> Ident.name)) {
             | Some(typeEnv1) => typeEnv1
             | None => typeEnv
             };
           let (dependencies, type_) =
             switch (moduleDeclaration.md_type) {
             | Mty_signature(signature) =>
               signature
               |> signatureToModuleRuntimeRepresentation(
                    ~config,
                    ~typeVarsGen,
                    ~typeEnv=typeEnv1,
                  )
             | Mty_ident(_)
             | Mty_functor(_)
             | Mty_alias(_) => ([], mixedOrUnknown(~config))
             };
           let field = {
             mutable_: Immutable,
             nameJS: id |> Ident.name,
             nameRE: id |> Ident.name,
             optional: Mandatory,
             type_,
           };
           (dependencies, [field]);

         | Types.Sig_type(_)
         | Types.Sig_typext(_)
         | Types.Sig_modtype(_)
         | Types.Sig_class(_)
         | Types.Sig_class_type(_) => ([], [])
         }
       );
  let (dependencies, fields) = {
    let (dl, fl) = dependenciesAndFields |> List.split;
    (dl |> List.concat, fl |> List.concat);
  };
  (
    dependencies,
    config.modulesAsObjects ? Object(Closed, fields) : Record(fields),
  );
};

let translateTypeExprFromTypes =
    (~config, ~noFunctionReturnDependencies=?, ~typeEnv, typeExpr) => {
  let typeVarsGen = GenIdent.createTypeVarsGen();
  let translation =
    typeExpr
    |> translateTypeExprFromTypes_(
         ~config,
         ~typeVarsGen,
         ~noFunctionReturnDependencies?,
         ~typeEnv,
       );

  if (Debug.dependencies^) {
    translation.dependencies
    |> List.iter(dep => Log_.item("Dependency: %s\n", dep |> depToString));
  };
  translation;
};

let translateTypeExprsFromTypes = (~config, ~typeEnv, typeExprs) => {
  let typeVarsGen = GenIdent.createTypeVarsGen();
  let translations =
    typeExprs |> translateTypeExprsFromTypes_(~config, ~typeVarsGen, ~typeEnv);

  if (Debug.dependencies^) {
    translations
    |> List.iter(translation =>
         translation.dependencies
         |> List.iter(dep =>
              Log_.item("Dependency: %s\n", dep |> depToString)
            )
       );
  };
  translations;
};