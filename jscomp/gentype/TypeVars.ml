open GenTypeCommon

let extractFromTypeExpr typeParams =
  typeParams
  |> List.fold_left
       (fun soFar typeExpr ->
         match typeExpr with
         | { Types.desc = Tvar (Some s) } ->
             let typeName = s in
             typeName :: soFar
         | { Types.desc = Tlink _ } ->
             (* see if we need to collect more type vars here: t as 'a *)
             soFar
         | _ -> assert false)
       []
  |> List.rev

let extractFromCoreType typeParams =
  typeParams
  |> List.fold_left
       (fun soFar typeExpr ->
         match typeExpr.Typedtree.ctyp_desc with
         | Ttyp_var s ->
             let typeName = s in
             typeName :: soFar
         | _ -> soFar)
       []
  |> List.rev

let rec substitute ~f type0 =
  match type0 with
  | Array (t, arrayKind) -> Array (t |> substitute ~f, arrayKind)
  | Function function_ ->
      Function
        {
          function_ with
          argTypes =
            function_.argTypes
            |> List.map (fun { aName; aType = t } ->
                   { aName; aType = t |> substitute ~f });
        }
  | GroupOfLabeledArgs fields ->
      GroupOfLabeledArgs
        (fields
        |> List.map (fun field ->
               { field with type_ = field.type_ |> substitute ~f }))
  | Ident { typeArgs = [] } -> type0
  | Ident ({ typeArgs } as ident) ->
      Ident { ident with typeArgs = typeArgs |> List.map (substitute ~f) }
  | Null type_ -> Null (type_ |> substitute ~f)
  | Nullable type_ -> Nullable (type_ |> substitute ~f)
  | Object (closedFlag, fields) ->
      Object
        ( closedFlag,
          fields
          |> List.map (fun field ->
                 { field with type_ = field.type_ |> substitute ~f }) )
  | Option type_ -> Option (type_ |> substitute ~f)
  | Promise type_ -> Promise (type_ |> substitute ~f)
  | Record fields ->
      Record
        (fields
        |> List.map (fun field ->
               { field with type_ = field.type_ |> substitute ~f }))
  | Tuple innerTypes -> Tuple (innerTypes |> List.map (substitute ~f))
  | TypeVar s -> ( match f s with None -> type0 | Some type1 -> type1)
  | Variant variant ->
      Variant
        {
          variant with
          payloads =
            variant.payloads
            |> List.map (fun payload ->
                   { payload with t = payload.t |> substitute ~f });
        }

let rec free_ type0 : StringSet.t =
  match type0 with
  | Array (t, _) -> t |> free_
  | Function { argTypes; retType; typeVars } ->
      StringSet.diff
        ((argTypes |> freeOfList_) +++ (retType |> free_))
        (typeVars |> StringSet.of_list)
  | GroupOfLabeledArgs fields | Object (_, fields) | Record fields ->
      fields
      |> List.fold_left
           (fun s { type_ } -> StringSet.union s (type_ |> free_))
           StringSet.empty
  | Ident { typeArgs } ->
      typeArgs
      |> List.fold_left
           (fun s typeArg -> StringSet.union s (typeArg |> free_))
           StringSet.empty
  | Null type_ | Nullable type_ -> type_ |> free_
  | Option type_ | Promise type_ -> type_ |> free_
  | Tuple innerTypes ->
      innerTypes
      |> List.fold_left
           (fun s typeArg -> StringSet.union s (typeArg |> free_))
           StringSet.empty
  | TypeVar s -> s |> StringSet.singleton
  | Variant { payloads } ->
      payloads
      |> List.fold_left
           (fun s { t } -> StringSet.union s (t |> free_))
           StringSet.empty

and freeOfList_ types =
  types
  |> List.fold_left (fun s { aType } -> s +++ (aType |> free_)) StringSet.empty

and ( +++ ) = StringSet.union

let free type_ = type_ |> free_ |> StringSet.elements
