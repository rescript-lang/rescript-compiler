open GenTypeCommon

let typeGetInlined ~config ~lookupId ~typeNameIsInterface type0 =
  let circular = ref "" in
  let rec visit ~(visited : StringSet.t) type_ =
    let normalized_ = type_ in
    match type_ with
    | Array (t, mutable_) ->
      let tNormalized = t |> visit ~visited in
      Array (tNormalized, mutable_)
    | Dict _ -> normalized_
    | Function ({argTypes; retType} as function_) ->
      let argConverted = argTypes |> List.map (argTypeToGroupedArg ~visited) in
      let retNormalized = retType |> visit ~visited in
      Function {function_ with argTypes = argConverted; retType = retNormalized}
    | Ident {builtin = true} -> normalized_
    | Ident {builtin = false; name; typeArgs} -> (
      if visited |> StringSet.mem name then (
        circular := name;
        normalized_)
      else
        let visited = visited |> StringSet.add name in
        match name |> lookupId with
        | {CodeItem.annotation = GenTypeOpaque} -> normalized_
        | {annotation = NoGenType} -> normalized_
        | {typeVars; type_} ->
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
          let inlined = type_ |> TypeVars.substitute ~f |> visit ~visited in
          inlined
        | exception Not_found ->
          let typeArgs = typeArgs |> List.map (fun t -> t |> visit ~visited) in
          Ident {builtin = false; name; typeArgs})
    | Null t ->
      let tNormalized = t |> visit ~visited in
      Null tNormalized
    | Nullable t ->
      let tNormalized = t |> visit ~visited in
      Nullable tNormalized
    | Object _ -> normalized_
    | Option t ->
      let tNormalized = t |> visit ~visited in
      Option tNormalized
    | Promise t ->
      let tNormalized = t |> visit ~visited in
      Promise tNormalized
    | Tuple innerTypes ->
      let normalizedList = innerTypes |> List.map (visit ~visited) in
      Tuple normalizedList
    | TypeVar _ -> normalized_
    | Variant variant ->
      let ordinaryVariant = not variant.polymorphic in
      let withPayloadConverted =
        variant.payloads
        |> List.map (fun (payload : payload) ->
               {payload with t = payload.t |> visit ~visited})
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
      normalized
  and argTypeToGroupedArg ~visited {aName; aType} =
    let tNormalized = aType |> visit ~visited in
    {aName; aType = tNormalized}
  in
  let normalized = type0 |> visit ~visited:StringSet.empty in
  if !Debug.converter then
    Log_.item "type0:%s \n"
      (type0 |> EmitType.typeToString ~config ~typeNameIsInterface);
  normalized
