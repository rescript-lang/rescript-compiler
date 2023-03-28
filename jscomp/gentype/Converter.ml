open GenTypeCommon

type t =
  | ArrayC of t
  | CircularC of string * t
  | FunctionC of functionC
  | IdentC
  | OptionC of t
  | PromiseC of t
  | TupleC of t list

and groupedArgConverter =
  | ArgConverter of t
  | GroupConverter of (string * optional * t) list

and functionC = {
  funArgConverters: groupedArgConverter list;
  componentName: string option;
  isHook: bool;
  retConverter: t;
  typeVars: string list;
  uncurried: bool;
}

let rec toString converter =
  match converter with
  | ArrayC c -> "array(" ^ toString c ^ ")"
  | CircularC (s, c) -> "circular(" ^ s ^ " " ^ toString c ^ ")"
  | FunctionC {funArgConverters; retConverter; uncurried} ->
    "fn"
    ^ (match uncurried with
      | true -> "Uncurried"
      | false -> "")
    ^ "("
    ^ (funArgConverters
      |> List.map (fun groupedArgConverter ->
             match groupedArgConverter with
             | ArgConverter conv -> "(" ^ "_" ^ ":" ^ toString conv ^ ")"
             | GroupConverter groupConverters ->
               "{|"
               ^ (groupConverters
                 |> List.map (fun (s, optional, argConverter) ->
                        s
                        ^ (match optional = Optional with
                          | true -> "?"
                          | false -> "")
                        ^ ":" ^ toString argConverter)
                 |> String.concat ", ")
               ^ "|}")
      |> String.concat ", ")
    ^ " -> " ^ toString retConverter ^ ")"
  | IdentC -> "id"
  | OptionC c -> "option(" ^ toString c ^ ")"
  | PromiseC c -> "promise(" ^ toString c ^ ")"
  | TupleC innerTypesC ->
    "[" ^ (innerTypesC |> List.map toString |> String.concat ", ") ^ "]"

let typeGetConverterNormalized ~config ~inline ~lookupId ~typeNameIsInterface
    type0 =
  let circular = ref "" in
  let expandOneLevel type_ =
    match type_ with
    | Ident {builtin = false; name} -> (
      match name |> lookupId with
      | (t : CodeItem.exportTypeItem) -> t.type_
      | exception Not_found -> type_)
    | _ -> type_
  in
  let rec visit ~(visited : StringSet.t) type_ =
    let normalized_ = type_ in
    match type_ with
    | Array (t, mutable_) ->
      let tConverter, tNormalized = t |> visit ~visited in
      (ArrayC tConverter, Array (tNormalized, mutable_))
    | Dict _ -> (IdentC, normalized_)
    | Function
        ({argTypes; componentName; retType; typeVars; uncurried} as function_)
      ->
      let argConverted =
        argTypes |> List.map (argTypeToGroupedArgConverter ~visited)
      in
      let funArgConverters = argConverted |> List.map fst in
      let retConverter, retNormalized = retType |> visit ~visited in
      let isHook =
        match argTypes with
        | [{aType = Object (_, fields)}] ->
          retType |> EmitType.isTypeFunctionComponent ~fields
        | _ -> false
      in
      ( FunctionC
          {
            funArgConverters;
            componentName;
            isHook;
            retConverter;
            typeVars;
            uncurried;
          },
        Function
          {
            function_ with
            argTypes = argConverted |> List.map snd;
            retType = retNormalized;
          } )
    | GroupOfLabeledArgs _ ->
      (* This case should only fire from withing a function *)
      (IdentC, normalized_)
    | Ident {builtin = true} -> (IdentC, normalized_)
    | Ident {builtin = false; name; typeArgs} -> (
      if visited |> StringSet.mem name then (
        circular := name;
        (IdentC, normalized_))
      else
        let visited = visited |> StringSet.add name in
        match name |> lookupId with
        | {annotation = GenTypeOpaque} -> (IdentC, normalized_)
        | {annotation = NoGenType} -> (IdentC, normalized_)
        | {typeVars; type_} -> (
          let pairs =
            try List.combine typeVars typeArgs with Invalid_argument _ -> []
          in
          let f typeVar =
            match
              pairs |> List.find (fun (typeVar1, _) -> typeVar = typeVar1)
            with
            | _, typeArgument -> Some typeArgument
            | exception Not_found -> None
          in
          let converter, inlined =
            type_ |> TypeVars.substitute ~f |> visit ~visited
          in
          ( converter,
            match inline with
            | true -> inlined
            | false -> normalized_ ))
        | exception Not_found ->
          if inline then
            let typeArgs =
              typeArgs |> List.map (fun t -> t |> visit ~visited |> snd)
            in
            (IdentC, Ident {builtin = false; name; typeArgs})
          else (IdentC, normalized_))
    | Null t ->
      let tConverter, tNormalized = t |> visit ~visited in
      (OptionC tConverter, Null tNormalized)
    | Nullable t ->
      let tConverter, tNormalized = t |> visit ~visited in
      (OptionC tConverter, Nullable tNormalized)
    | Object _ -> (IdentC, normalized_)
    | Option t ->
      let tConverter, tNormalized = t |> visit ~visited in
      (OptionC tConverter, Option tNormalized)
    | Promise t ->
      let tConverter, tNormalized = t |> visit ~visited in
      (PromiseC tConverter, Promise tNormalized)
    | Tuple innerTypes ->
      let innerConversions, normalizedList =
        innerTypes |> List.map (visit ~visited) |> List.split
      in
      (TupleC innerConversions, Tuple normalizedList)
    | TypeVar _ -> (IdentC, normalized_)
    | Variant variant ->
      let ordinaryVariant = not variant.polymorphic in
      let withPayloadConverted =
        variant.payloads
        |> List.map (fun (payload : payload) ->
               {payload with t = snd (payload.t |> visit ~visited)})
      in
      let normalized =
        match withPayloadConverted with
        | [] when ordinaryVariant -> normalized_
        | [payload] when ordinaryVariant ->
          let _unboxed = payload.t |> expandOneLevel |> typeIsObject in
          let normalized = Variant {variant with payloads = [payload]} in
          normalized
        | withPayloadConverted ->
          Variant {variant with payloads = withPayloadConverted}
      in
      (IdentC, normalized)
  and argTypeToGroupedArgConverter ~visited {aName; aType} =
    match aType with
    | GroupOfLabeledArgs fields ->
      let fieldsConverted =
        fields
        |> List.map (fun ({type_} as field) -> (field, type_ |> visit ~visited))
      in
      let tNormalized =
        GroupOfLabeledArgs
          (fieldsConverted
          |> List.map (fun (field, (_, t)) -> {field with type_ = t}))
      in
      let converter =
        GroupConverter
          (fieldsConverted
          |> List.map (fun ({nameJS; optional}, (converter, _)) ->
                 (nameJS, optional, converter)))
      in
      (converter, {aName; aType = tNormalized})
    | _ ->
      let converter, tNormalized = aType |> visit ~visited in
      let converter = ArgConverter converter in
      (converter, {aName; aType = tNormalized})
  in
  let converter, normalized = type0 |> visit ~visited:StringSet.empty in
  let finalConverter =
    match !circular <> "" with
    | true -> CircularC (!circular, converter)
    | false -> converter
  in
  if !Debug.converter then
    Log_.item "Converter type0:%s converter:%s\n"
      (type0 |> EmitType.typeToString ~config ~typeNameIsInterface)
      (finalConverter |> toString);
  (finalConverter, normalized)

let typeGetConverter ~config ~lookupId ~typeNameIsInterface type_ =
  type_
  |> typeGetConverterNormalized ~config ~inline:false ~lookupId
       ~typeNameIsInterface
  |> fst

let typeGetNormalized ~config ~inline ~lookupId ~typeNameIsInterface type_ =
  type_
  |> typeGetConverterNormalized ~config ~inline ~lookupId ~typeNameIsInterface
  |> snd

let rec converterIsIdentity ~config ~toJS converter =
  match converter with
  | ArrayC c -> c |> converterIsIdentity ~config ~toJS
  | CircularC (_, c) -> c |> converterIsIdentity ~config ~toJS
  | FunctionC {funArgConverters; retConverter; uncurried} ->
    retConverter |> converterIsIdentity ~config ~toJS
    && ((not toJS) || uncurried || funArgConverters |> List.length <= 1)
    && funArgConverters
       |> List.for_all (fun groupedArgConverter ->
              match groupedArgConverter with
              | ArgConverter argConverter ->
                argConverter |> converterIsIdentity ~config ~toJS:(not toJS)
              | GroupConverter _ -> false)
  | IdentC -> true
  | OptionC c -> c |> converterIsIdentity ~config ~toJS
  | PromiseC c -> c |> converterIsIdentity ~config ~toJS
  | TupleC innerTypesC ->
    innerTypesC |> List.for_all (converterIsIdentity ~config ~toJS)

let rec apply ~config ~converter ~indent ~nameGen ~toJS ~variantTables value =
  match converter with
  | _ when converter |> converterIsIdentity ~config ~toJS -> value
  | ArrayC c ->
    let x = "ArrayItem" |> EmitText.name ~nameGen in
    value ^ ".map(function _element("
    ^ (x |> EmitType.ofTypeAny ~config)
    ^ ") { return "
    ^ (x |> apply ~config ~converter:c ~indent ~nameGen ~toJS ~variantTables)
    ^ "})"
  | CircularC (s, c) ->
    value
    |> EmitText.addComment
         ~comment:
           ("WARNING: circular type " ^ s ^ ". Only shallow converter applied.")
    |> apply ~config ~converter:c ~indent ~nameGen ~toJS ~variantTables
  | FunctionC
      {
        funArgConverters;
        componentName;
        isHook;
        retConverter;
        typeVars;
        uncurried;
      } ->
    let resultName = EmitText.resultName ~nameGen in
    let indent1 = indent |> Indent.more in
    let indent2 = indent1 |> Indent.more in
    let mkReturn x =
      "const " ^ resultName ^ " = " ^ x ^ ";"
      ^ Indent.break ~indent:indent1
      ^ "return "
      ^ (resultName
        |> apply ~config ~converter:retConverter ~indent:indent2 ~nameGen ~toJS
             ~variantTables)
    in
    let convertArg i groupedArgConverter =
      match groupedArgConverter with
      | ArgConverter argConverter ->
        let varName = i + 1 |> EmitText.argi ~nameGen in
        let notToJS = not toJS in
        ( [varName],
          [
            varName
            |> apply ~config ~converter:argConverter ~indent:indent2 ~nameGen
                 ~toJS:notToJS ~variantTables;
          ] )
      | GroupConverter groupConverters ->
        let notToJS = not toJS in
        if toJS then
          let varName = i + 1 |> EmitText.argi ~nameGen in
          ( [varName],
            groupConverters
            |> List.map (fun (label, optional, argConverter) ->
                   varName
                   |> EmitText.fieldAccess ~label
                   |> apply ~config
                        ~converter:
                          (match
                             optional = Optional
                             && not
                                  (argConverter
                                  |> converterIsIdentity ~config ~toJS:notToJS)
                           with
                          | true -> OptionC argConverter
                          | false -> argConverter)
                        ~indent:indent2 ~nameGen ~toJS:notToJS ~variantTables)
          )
        else
          let varNames =
            groupConverters
            |> List.map (fun (s, _optional, _argConverter) ->
                   s |> EmitText.arg ~nameGen)
          in
          let varNamesArr = varNames |> Array.of_list in
          let fieldValues =
            groupConverters
            |> List.mapi (fun i (s, _optional, argConverter) ->
                   s ^ ":"
                   ^ ((varNamesArr.(i) [@doesNotRaise])
                     |> apply ~config ~converter:argConverter ~indent:indent2
                          ~nameGen ~toJS:notToJS ~variantTables))
            |> String.concat ", "
          in
          (varNames, ["{" ^ fieldValues ^ "}"])
    in
    let mkBody bodyArgs =
      let useCurry = (not uncurried) && toJS && List.length bodyArgs > 1 in
      config.emitImportCurry <- config.emitImportCurry || useCurry;
      let functionName =
        match isHook with
        | true -> "React.createElement"
        | false -> value
      in
      if isHook then config.emitImportReact <- true;
      let declareProps, args =
        match bodyArgs with
        | [props] when isHook ->
          let propsName = "$props" |> EmitText.name ~nameGen in
          ( Indent.break ~indent:indent1
            ^ "const " ^ propsName ^ " = " ^ props ^ ";",
            [value; propsName] )
        | _ -> ("", bodyArgs)
      in
      declareProps
      ^ Indent.break ~indent:indent1
      ^ (functionName |> EmitText.funCall ~args ~useCurry |> mkReturn)
    in
    let convertedArgs = funArgConverters |> List.mapi convertArg in
    let args = convertedArgs |> List.map fst |> List.concat in
    let funParams =
      args |> List.map (fun v -> v |> EmitType.ofTypeAny ~config)
    in
    let bodyArgs = convertedArgs |> List.map snd |> List.concat in
    EmitText.funDef ~bodyArgs ~functionName:componentName ~funParams ~indent
      ~mkBody ~typeVars
  | IdentC -> value
  | OptionC c ->
    EmitText.parens
      [
        value ^ " == null ? " ^ value ^ " : "
        ^ (value
          |> apply ~config ~converter:c ~indent ~nameGen ~toJS ~variantTables);
      ]
  | PromiseC c ->
    let x = "$promise" |> EmitText.name ~nameGen in
    value ^ ".then(function _element("
    ^ (x |> EmitType.ofTypeAny ~config)
    ^ ") { return "
    ^ (x |> apply ~config ~converter:c ~indent ~nameGen ~toJS ~variantTables)
    ^ "})"
  | TupleC innerTypesC ->
    "["
    ^ (innerTypesC
      |> List.mapi (fun index c ->
             value
             |> EmitText.arrayAccess ~index
             |> apply ~config ~converter:c ~indent ~nameGen ~toJS ~variantTables)
      |> String.concat ", ")
    ^ "]"

let toJS ~config ~converter ~indent ~nameGen ~variantTables value =
  value |> apply ~config ~converter ~indent ~nameGen ~variantTables ~toJS:true

let toReason ~config ~converter ~indent ~nameGen ~variantTables value =
  value |> apply ~config ~converter ~indent ~nameGen ~toJS:false ~variantTables
