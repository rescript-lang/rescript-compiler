open GenTypeCommon

type t =
  | ArrayC of t
  | CircularC of string * t
  | FunctionC of functionC
  | IdentC
  | NullableC of t
  | ObjectC of fieldsC
  | OptionC of t
  | PromiseC of t
  | RecordC of fieldsC
  | TupleC of t list
  | VariantC of variantC

and groupedArgConverter =
  | ArgConverter of t
  | GroupConverter of (string * optional * t) list

and fieldC = { lblJS : string; lblRE : string; c : t }
and fieldsC = fieldC list

and functionC = {
  funArgConverters : groupedArgConverter list;
  componentName : string option;
  isHook : bool;
  retConverter : t;
  typeVars : string list;
  uncurried : bool;
}

and variantC = {
  hash : int;
  noPayloads : case list;
  withPayloads : withPayload list;
  polymorphic : bool;
  unboxed : bool;
  useVariantTables : bool;
}

and withPayload = { case : case; inlineRecord : bool; argConverters : t list }

let rec toString converter =
  match converter with
  | ArrayC c -> "array(" ^ toString c ^ ")"
  | CircularC (s, c) -> "circular(" ^ s ^ " " ^ toString c ^ ")"
  | FunctionC { funArgConverters; retConverter; uncurried } ->
      "fn"
      ^ (match uncurried with true -> "Uncurried" | false -> "")
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
  | NullableC c -> "nullable(" ^ toString c ^ ")"
  | ObjectC fieldsC | RecordC fieldsC ->
      let dot = match converter with ObjectC _ -> ". " | _ -> "" in
      "{" ^ dot
      ^ (fieldsC
        |> List.map (fun { lblJS; lblRE; c } ->
               (match lblJS = lblRE with
               | true -> lblJS
               | false -> "(" ^ lblJS ^ "/" ^ lblRE ^ ")")
               ^ ":" ^ (c |> toString))
        |> String.concat ", ")
      ^ "}"
  | OptionC c -> "option(" ^ toString c ^ ")"
  | PromiseC c -> "promise(" ^ toString c ^ ")"
  | TupleC innerTypesC ->
      "[" ^ (innerTypesC |> List.map toString |> String.concat ", ") ^ "]"
  | VariantC { noPayloads; withPayloads } ->
      "variant("
      ^ ((noPayloads |> List.map labelJSToString)
         @ (withPayloads
           |> List.map (fun { case; inlineRecord; argConverters } ->
                  (case |> labelJSToString)
                  ^ (match inlineRecord with
                    | true -> " inlineRecord "
                    | false -> "")
                  ^ ":" ^ "{"
                  ^ (argConverters |> List.map toString |> String.concat ", ")
                  ^ "}"))
        |> String.concat ", ")
      ^ ")"

let typeGetConverterNormalized ~config ~inline ~lookupId ~typeNameIsInterface
    type0 =
  let circular = ref "" in
  let expandOneLevel type_ =
    match type_ with
    | Ident { builtin = false; name } -> (
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
    | Function
        ({ argTypes; componentName; retType; typeVars; uncurried } as function_)
      ->
        let argConverted =
          argTypes |> List.map (argTypeToGroupedArgConverter ~visited)
        in
        let funArgConverters = argConverted |> List.map fst in
        let retConverter, retNormalized = retType |> visit ~visited in
        let isHook =
          match argTypes with
          | [ { aType = Object (_, fields) } ] ->
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
    | Ident { builtin = true } -> (IdentC, normalized_)
    | Ident { builtin = false; name; typeArgs } -> (
        if visited |> StringSet.mem name then (
          circular := name;
          (IdentC, normalized_))
        else
          let visited = visited |> StringSet.add name in
          match name |> lookupId with
          | { annotation = GenTypeOpaque } -> (IdentC, normalized_)
          | { annotation = NoGenType } -> (IdentC, normalized_)
          | { typeVars; type_ } -> (
              let pairs =
                try List.combine typeVars typeArgs
                with Invalid_argument _ -> []
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
                match inline with true -> inlined | false -> normalized_ ))
          | exception Not_found ->
              if inline then
                let typeArgs =
                  typeArgs |> List.map (fun t -> t |> visit ~visited |> snd)
                in
                (IdentC, Ident { builtin = false; name; typeArgs })
              else (IdentC, normalized_))
    | Null t ->
        let tConverter, tNormalized = t |> visit ~visited in
        (NullableC tConverter, Null tNormalized)
    | Nullable t ->
        let tConverter, tNormalized = t |> visit ~visited in
        (NullableC tConverter, Nullable tNormalized)
    | Object (closedFlag, fields) ->
        let fieldsConverted =
          fields
          |> List.map (fun ({ type_ } as field) ->
                 (field, type_ |> visit ~visited))
        in
        ( ObjectC
            (fieldsConverted
            |> List.map (fun ({ nameJS; nameRE; optional }, (converter, _)) ->
                   {
                     lblJS = nameJS;
                     lblRE = nameRE;
                     c =
                       (match optional = Mandatory with
                       | true -> converter
                       | false -> OptionC converter);
                   })),
          Object
            ( closedFlag,
              fieldsConverted
              |> List.map (fun (field, (_, tNormalized)) ->
                     { field with type_ = tNormalized }) ) )
    | Option t ->
        let tConverter, tNormalized = t |> visit ~visited in
        (OptionC tConverter, Option tNormalized)
    | Promise t ->
        let tConverter, tNormalized = t |> visit ~visited in
        (PromiseC tConverter, Promise tNormalized)
    | Record fields ->
        let fieldsConverted =
          fields
          |> List.map (fun ({ type_ } as field) ->
                 (field, type_ |> visit ~visited))
        in
        ( RecordC
            (fieldsConverted
            |> List.map (fun ({ nameJS; nameRE; optional }, (converter, _)) ->
                   {
                     lblJS = nameJS;
                     lblRE = nameRE;
                     c =
                       (match optional = Mandatory with
                       | true -> converter
                       | false -> OptionC converter);
                   })),
          Record
            (fieldsConverted
            |> List.map (fun (field, (_, tNormalized)) ->
                   { field with type_ = tNormalized })) )
    | Tuple innerTypes ->
        let innerConversions, normalizedList =
          innerTypes |> List.map (visit ~visited) |> List.split
        in
        (TupleC innerConversions, Tuple normalizedList)
    | TypeVar _ -> (IdentC, normalized_)
    | Variant variant ->
        let allowUnboxed = not variant.polymorphic in
        let withPayloads, normalized, unboxed =
          match
            variant.payloads
            |> List.map (fun { case; inlineRecord; numArgs; t } ->
                   (case, inlineRecord, numArgs, t |> visit ~visited))
          with
          | [] when allowUnboxed -> ([], normalized_, variant.unboxed)
          | [ (case, inlineRecord, numArgs, (converter, tNormalized)) ]
            when allowUnboxed ->
              let unboxed = tNormalized |> expandOneLevel |> typeIsObject in
              let normalized =
                Variant
                  {
                    variant with
                    payloads =
                      [ { case; inlineRecord; numArgs; t = tNormalized } ];
                    unboxed =
                      (match unboxed with
                      | true -> true
                      | false -> variant.unboxed);
                  }
              in
              let argConverters =
                match converter with
                | TupleC converters when numArgs > 1 -> converters
                | _ -> [ converter ]
              in
              ([ { argConverters; case; inlineRecord } ], normalized, unboxed)
          | withPayloadConverted ->
              let withPayloadNormalized =
                withPayloadConverted
                |> List.map
                     (fun (case, inlineRecord, numArgs, (_, tNormalized)) ->
                       { case; inlineRecord; numArgs; t = tNormalized })
              in
              let normalized =
                Variant { variant with payloads = withPayloadNormalized }
              in
              ( withPayloadConverted
                |> List.map
                     (fun (case, inlineRecord, numArgs, (converter, _)) ->
                       let argConverters =
                         match converter with
                         | TupleC converters when numArgs > 1 -> converters
                         | _ -> [ converter ]
                       in
                       { argConverters; case; inlineRecord }),
                normalized,
                variant.unboxed )
        in
        let noPayloads = variant.noPayloads in
        let useVariantTables =
          if variant.bsStringOrInt then false
          else if variant.polymorphic then
            noPayloads
            |> List.exists (fun { label; labelJS } ->
                   labelJS <> StringLabel label)
            || withPayloads
               |> List.exists (fun { case = { label; labelJS } } ->
                      labelJS <> StringLabel label)
          else true
        in
        let converter =
          VariantC
            {
              hash = variant.hash;
              noPayloads;
              withPayloads;
              polymorphic = variant.polymorphic;
              unboxed;
              useVariantTables;
            }
        in
        (converter, normalized)
  and argTypeToGroupedArgConverter ~visited { aName; aType } =
    match aType with
    | GroupOfLabeledArgs fields ->
        let fieldsConverted =
          fields
          |> List.map (fun ({ type_ } as field) ->
                 (field, type_ |> visit ~visited))
        in
        let tNormalized =
          GroupOfLabeledArgs
            (fieldsConverted
            |> List.map (fun (field, (_, t)) -> { field with type_ = t }))
        in
        let converter =
          GroupConverter
            (fieldsConverted
            |> List.map (fun ({ nameJS; optional }, (converter, _)) ->
                   (nameJS, optional, converter)))
        in
        (converter, { aName; aType = tNormalized })
    | _ ->
        let converter, tNormalized = aType |> visit ~visited in
        let converter = ArgConverter converter in
        (converter, { aName; aType = tNormalized })
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
  | FunctionC { funArgConverters; retConverter; uncurried } ->
      retConverter |> converterIsIdentity ~config ~toJS
      && ((not toJS) || uncurried || funArgConverters |> List.length <= 1)
      && funArgConverters
         |> List.for_all (fun groupedArgConverter ->
                match groupedArgConverter with
                | ArgConverter argConverter ->
                    argConverter |> converterIsIdentity ~config ~toJS:(not toJS)
                | GroupConverter _ -> false)
  | IdentC -> true
  | NullableC c -> c |> converterIsIdentity ~config ~toJS
  | ObjectC fieldsC ->
      fieldsC
      |> List.for_all (fun { lblJS; lblRE; c } ->
             lblJS = lblRE
             &&
             match c with
             | OptionC c1 -> c1 |> converterIsIdentity ~config ~toJS
             | _ -> c |> converterIsIdentity ~config ~toJS)
  | OptionC c -> if toJS then c |> converterIsIdentity ~config ~toJS else false
  | PromiseC c -> c |> converterIsIdentity ~config ~toJS
  | RecordC _ -> false
  | TupleC innerTypesC ->
      innerTypesC |> List.for_all (converterIsIdentity ~config ~toJS)
  | VariantC { withPayloads; useVariantTables } ->
      if not useVariantTables then
        withPayloads
        |> List.for_all (fun { argConverters } ->
               argConverters
               |> List.for_all (fun c -> c |> converterIsIdentity ~config ~toJS))
      else false

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
             ("WARNING: circular type " ^ s
            ^ ". Only shallow converter applied.")
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
          |> apply ~config ~converter:retConverter ~indent:indent2 ~nameGen
               ~toJS ~variantTables)
      in
      let convertArg i groupedArgConverter =
        match groupedArgConverter with
        | ArgConverter argConverter ->
            let varName = i + 1 |> EmitText.argi ~nameGen in
            let notToJS = not toJS in
            ( [ varName ],
              [
                varName
                |> apply ~config ~converter:argConverter ~indent:indent2
                     ~nameGen ~toJS:notToJS ~variantTables;
              ] )
        | GroupConverter groupConverters ->
            let notToJS = not toJS in
            if toJS then
              let varName = i + 1 |> EmitText.argi ~nameGen in
              ( [ varName ],
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
                                      |> converterIsIdentity ~config
                                           ~toJS:notToJS)
                               with
                              | true -> OptionC argConverter
                              | false -> argConverter)
                            ~indent:indent2 ~nameGen ~toJS:notToJS
                            ~variantTables) )
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
                       ^ (varNamesArr.(i)
                         |> apply ~config ~converter:argConverter
                              ~indent:indent2 ~nameGen ~toJS:notToJS
                              ~variantTables))
                |> String.concat ", "
              in
              (varNames, [ "{" ^ fieldValues ^ "}" ])
      in
      let mkBody bodyArgs =
        let useCurry = (not uncurried) && toJS && List.length bodyArgs > 1 in
        config.emitImportCurry <- config.emitImportCurry || useCurry;
        let functionName =
          match isHook with true -> "React.createElement" | false -> value
        in
        if isHook then config.emitImportReact <- true;
        let declareProps, args =
          match bodyArgs with
          | [ props ] when isHook ->
              let propsName = "$props" |> EmitText.name ~nameGen in
              ( Indent.break ~indent:indent1
                ^ "const " ^ propsName ^ " = " ^ props ^ ";",
                [ value; propsName ] )
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
  | NullableC c ->
      EmitText.parens
        [
          value ^ " == null ? " ^ value ^ " : "
          ^ (value
            |> apply ~config ~converter:c ~indent ~nameGen ~toJS ~variantTables
            );
        ]
  | ObjectC fieldsC ->
      let simplifyFieldConverted fieldConverter =
        match fieldConverter with
        | OptionC converter1
          when converter1 |> converterIsIdentity ~config ~toJS ->
            IdentC
        | _ -> fieldConverter
      in
      let fieldValues =
        fieldsC
        |> List.map (fun { lblJS; lblRE; c = fieldConverter } ->
               (match toJS with true -> lblJS | false -> lblRE)
               ^ ":"
               ^ (value
                 |> EmitText.fieldAccess
                      ~label:(match toJS with true -> lblRE | false -> lblJS)
                 |> apply ~config
                      ~converter:(fieldConverter |> simplifyFieldConverted)
                      ~indent ~nameGen ~toJS ~variantTables))
        |> String.concat ", "
      in
      "{" ^ fieldValues ^ "}"
  | OptionC c ->
      if toJS then
        EmitText.parens
          [
            value ^ " == null ? " ^ value ^ " : "
            ^ (value
              |> apply ~config ~converter:c ~indent ~nameGen ~toJS
                   ~variantTables);
          ]
      else
        EmitText.parens
          [
            value ^ " == null ? undefined : "
            ^ (value
              |> apply ~config ~converter:c ~indent ~nameGen ~toJS
                   ~variantTables);
          ]
  | PromiseC c ->
      let x = "$promise" |> EmitText.name ~nameGen in
      value ^ ".then(function _element("
      ^ (x |> EmitType.ofTypeAny ~config)
      ^ ") { return "
      ^ (x |> apply ~config ~converter:c ~indent ~nameGen ~toJS ~variantTables)
      ^ "})"
  | RecordC fieldsC ->
      let simplifyFieldConverted fieldConverter =
        match fieldConverter with
        | OptionC converter1
          when converter1 |> converterIsIdentity ~config ~toJS ->
            IdentC
        | _ -> fieldConverter
      in
      if toJS then
        let fieldValues =
          fieldsC
          |> List.mapi (fun index { lblJS; c = fieldConverter } ->
                 lblJS ^ ":"
                 ^ (value
                   |> EmitText.arrayAccess ~index
                   |> apply ~config
                        ~converter:(fieldConverter |> simplifyFieldConverted)
                        ~indent ~nameGen ~toJS ~variantTables))
          |> String.concat ", "
        in
        "{" ^ fieldValues ^ "}"
      else
        let fieldValues =
          fieldsC
          |> List.map (fun { lblJS; c = fieldConverter } ->
                 value
                 |> EmitText.fieldAccess ~label:lblJS
                 |> apply ~config
                      ~converter:(fieldConverter |> simplifyFieldConverted)
                      ~indent ~nameGen ~toJS ~variantTables)
          |> String.concat ", "
        in
        "[" ^ fieldValues ^ "]"
  | TupleC innerTypesC ->
      "["
      ^ (innerTypesC
        |> List.mapi (fun index c ->
               value
               |> EmitText.arrayAccess ~index
               |> apply ~config ~converter:c ~indent ~nameGen ~toJS
                    ~variantTables)
        |> String.concat ", ")
      ^ "]"
  | VariantC { noPayloads = [ case ]; withPayloads = []; polymorphic } -> (
      match toJS with
      | true -> case |> labelJSToString
      | false -> case.label |> Runtime.emitVariantLabel ~polymorphic)
  | VariantC variantC -> (
      if variantC.noPayloads <> [] && variantC.useVariantTables then
        Hashtbl.replace variantTables (variantC.hash, toJS) variantC;
      let convertToString =
        match
          (not toJS)
          && variantC.noPayloads
             |> List.exists (fun { labelJS } ->
                    labelJS = BoolLabel true || labelJS = BoolLabel false)
        with
        | true -> ".toString()"
        | false -> ""
      in
      let table = variantC.hash |> variantTable ~toJS in
      let accessTable v =
        match not variantC.useVariantTables with
        | true -> v
        | false -> table ^ EmitText.array [ v ^ convertToString ]
      in
      let convertVariantPayloadToJS ~indent ~argConverters x =
        match argConverters with
        | [ converter ] ->
            x |> apply ~config ~converter ~indent ~nameGen ~toJS ~variantTables
        | _ ->
            argConverters
            |> List.mapi (fun i converter ->
                   x
                   |> Runtime.accessVariant ~index:i
                   |> apply ~config ~converter ~indent ~nameGen ~toJS
                        ~variantTables)
            |> EmitText.array
      in
      let convertVariantPayloadToRE ~indent ~argConverters x =
        match argConverters with
        | [ converter ] ->
            [
              x
              |> apply ~config ~converter ~indent ~nameGen ~toJS ~variantTables;
            ]
        | _ ->
            argConverters
            |> List.mapi (fun i converter ->
                   x
                   |> EmitText.arrayAccess ~index:i
                   |> apply ~config ~converter ~indent ~nameGen ~toJS
                        ~variantTables)
      in
      match variantC.withPayloads with
      | [] -> value |> accessTable
      | [ { case; inlineRecord; argConverters } ] when variantC.unboxed -> (
          let casesWithPayload ~indent =
            if toJS then
              value
              |> Runtime.emitVariantGetPayload ~inlineRecord
                   ~numArgs:(argConverters |> List.length)
                   ~polymorphic:variantC.polymorphic
              |> convertVariantPayloadToJS ~argConverters ~indent
            else
              value
              |> convertVariantPayloadToRE ~argConverters ~indent
              |> Runtime.emitVariantWithPayload ~inlineRecord ~label:case.label
                   ~polymorphic:variantC.polymorphic
          in
          match variantC.noPayloads = [] with
          | true -> casesWithPayload ~indent
          | false ->
              EmitText.ifThenElse ~indent
                (fun ~indent:_ -> value |> EmitText.typeOfObject)
                casesWithPayload
                (fun ~indent:_ -> value |> accessTable))
      | _ :: _ -> (
          let convertCaseWithPayload ~indent ~inlineRecord ~argConverters case =
            if toJS then
              value
              |> Runtime.emitVariantGetPayload ~inlineRecord
                   ~numArgs:(argConverters |> List.length)
                   ~polymorphic:variantC.polymorphic
              |> convertVariantPayloadToJS ~argConverters ~indent
              |> Runtime.emitJSVariantWithPayload
                   ~label:(case |> labelJSToString)
                   ~polymorphic:variantC.polymorphic
            else
              value
              |> Runtime.emitJSVariantGetPayload
                   ~polymorphic:variantC.polymorphic
              |> convertVariantPayloadToRE ~argConverters ~indent
              |> Runtime.emitVariantWithPayload ~inlineRecord ~label:case.label
                   ~polymorphic:variantC.polymorphic
          in
          let switchCases ~indent =
            variantC.withPayloads
            |> List.map (fun { case; inlineRecord; argConverters } ->
                   ( (match toJS with
                     | true ->
                         case.label
                         |> Runtime.emitVariantLabel
                              ~polymorphic:variantC.polymorphic
                     | false -> case |> labelJSToString),
                     case
                     |> convertCaseWithPayload ~indent ~inlineRecord
                          ~argConverters ))
          in
          let casesWithPayload ~indent =
            value
            |> (let open Runtime in
               (match toJS with
               | true -> emitVariantGetLabel
               | false -> emitJSVariantGetLabel)
                 ~polymorphic:variantC.polymorphic)
            |> EmitText.switch ~indent ~cases:(switchCases ~indent)
          in
          match variantC.noPayloads = [] with
          | true -> casesWithPayload ~indent
          | false ->
              EmitText.ifThenElse ~indent
                (fun ~indent:_ -> value |> EmitText.typeOfObject)
                casesWithPayload
                (fun ~indent:_ -> value |> accessTable)))

let toJS ~config ~converter ~indent ~nameGen ~variantTables value =
  value |> apply ~config ~converter ~indent ~nameGen ~variantTables ~toJS:true

let toReason ~config ~converter ~indent ~nameGen ~variantTables value =
  value |> apply ~config ~converter ~indent ~nameGen ~toJS:false ~variantTables
