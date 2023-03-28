open GenTypeCommon

type t = IdentC

and groupedArgConverter =
  | ArgConverter of t
  | GroupConverter of (string * optional * t) list

let toString converter =
  match converter with
  | IdentC -> "id"

let typeGetConverterNormalized ~config ~inline ~lookupId ~typeNameIsInterface
    type0 =
  let circular = ref "" in
  let rec visit ~(visited : StringSet.t) type_ =
    let normalized_ = type_ in
    match type_ with
    | Array (t, mutable_) ->
      let _, tNormalized = t |> visit ~visited in
      (IdentC, Array (tNormalized, mutable_))
    | Dict _ -> (IdentC, normalized_)
    | Function ({argTypes; retType} as function_) ->
      let argConverted =
        argTypes |> List.map (argTypeToGroupedArgConverter ~visited)
      in
      let _, retNormalized = retType |> visit ~visited in
      ( IdentC,
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
        | {CodeItem.annotation = GenTypeOpaque} -> (IdentC, normalized_)
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
      let _, tNormalized = t |> visit ~visited in
      (IdentC, Null tNormalized)
    | Nullable t ->
      let _, tNormalized = t |> visit ~visited in
      (IdentC, Nullable tNormalized)
    | Object _ -> (IdentC, normalized_)
    | Option t ->
      let _, tNormalized = t |> visit ~visited in
      (IdentC, Option tNormalized)
    | Promise t ->
      let _, tNormalized = t |> visit ~visited in
      (IdentC, Promise tNormalized)
    | Tuple innerTypes ->
      let _, normalizedList =
        innerTypes |> List.map (visit ~visited) |> List.split
      in
      (IdentC, Tuple normalizedList)
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
  if !Debug.converter then
    Log_.item "Converter type0:%s converter:%s\n"
      (type0 |> EmitType.typeToString ~config ~typeNameIsInterface)
      (converter |> toString);
  (converter, normalized)

let typeGetConverter ~config ~lookupId ~typeNameIsInterface type_ =
  type_
  |> typeGetConverterNormalized ~config ~inline:false ~lookupId
       ~typeNameIsInterface
  |> fst

let typeGetNormalized ~config ~inline ~lookupId ~typeNameIsInterface type_ =
  type_
  |> typeGetConverterNormalized ~config ~inline ~lookupId ~typeNameIsInterface
  |> snd
