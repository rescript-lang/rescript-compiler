
module Decoder = struct

  type error = String.t

  module ObjectDict = Map.Make(String)

  type ('input, 'result) t =
    Decoder of ('input -> ('result, error) Tea_result.t)
    (*
    | Parser : (Web.Json.t -> ('result, error) Tea_result.t) -> ('result, error) Tea_result.t t
    *)
    (*
    | Value : (Web.Json.t, error) Tea_result.t t
    | Succeed : 'result -> ('result, error) Tea_result.t t
    | Fail : error -> (_, error) Tea_result.t t
    | Null : 'result -> ('result, error) Tea_result.t t
    | String : (string, error) Tea_result.t t
    *)

  exception ParseFail of string


  (* Primitive types *)

  let string =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONString s -> Tea_result.Ok s
          | _ -> Tea_result.Error "Non-string value"
      )

  let int =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONNumber n ->
            if n > (float_of_int min_int) && n < (float_of_int max_int)
            then Tea_result.Ok (int_of_float n)
            else Tea_result.Error "number out of int range"
          | _ -> Tea_result.Error "Non-int value"
      )

  let float =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONNumber n -> Tea_result.Ok n
          | _ -> Tea_result.Error "Non-float-value"
      )

  let bool =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONTrue -> Tea_result.Ok true
          | JSONFalse -> Tea_result.Ok false
          | _ -> Tea_result.Error "Non-boolean value"
      )

  let null v =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONNull -> Tea_result.Ok v
          | _ -> Tea_result.Error "Non-null value"
      )

  (* Compound types *)

  let list (Decoder decoder) =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONArray a ->
            ( let parse v =
                ( match decoder v with
                  | Tea_result.Ok r -> r
                  | Tea_result.Error e -> raise (ParseFail e)
                ) in
              try Tea_result.Ok (Array.to_list a |> List.map parse)
              with ParseFail e -> Tea_result.Error ("list -> " ^ e)
            )
          | _ -> Tea_result.Error "Non-list value"
      )

  let array (Decoder decoder) =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONArray a ->
            ( let parse v =
                ( match decoder v with
                  | Tea_result.Ok r -> r
                  | Tea_result.Error e -> raise (ParseFail e)
                ) in
              try Tea_result.Ok (Array.map parse a)
              with ParseFail e -> Tea_result.Error ("array -> " ^ e)
            )
          | _ -> Tea_result.Error "Non-array value"
      )

  let keyValuePairs (Decoder decoder) =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONObject o ->
            ( let keys = Js.Dict.keys o in
              let parse k l =
                ( match Js.Dict.get o k with
                  | None -> raise (ParseFail ("Key is undefined: " ^ k))
                  | Some v ->
                    match decoder v with
                    | Tea_result.Ok r -> (k, r) :: l
                    | Tea_result.Error e -> raise (ParseFail e)
                ) in
              try Tea_result.Ok (Array.fold_right parse keys [])
              with ParseFail e -> Tea_result.Error ("Invalid keyValuePair parsing: " ^ e)
            )
          | _ -> Tea_result.Error "Non-keyValuePair value"
      )

  let dict (Decoder decoder) =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONObject o ->
            ( let keys = Js.Dict.keys o in
              let parse k d =
                ( match Js.Dict.get o k with
                  | None -> raise (ParseFail ("Key is undefined: " ^ k))
                  | Some v ->
                    match decoder v with
                    | Tea_result.Ok r -> ObjectDict.add k r d
                    | Tea_result.Error e -> raise (ParseFail e)
                ) in
              let emptyDict = ObjectDict.empty in
              try Tea_result.Ok (Array.fold_right parse keys emptyDict)
              with ParseFail e -> Tea_result.Error ("Invalid dict parsing: " ^ e)
            )
          | _ -> Tea_result.Error "Non-dict value"
      )

  let field key (Decoder decoder) =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONObject o ->
            ( match Js.Dict.get o key with
              | None -> Tea_result.Error ("Field Value is undefined: " ^ key)
              | Some v ->
                match decoder v with
                | Ok _ as o -> o
                | Error e -> Error ("field `" ^ key ^ "` -> " ^ e)
            )
          | _ -> Tea_result.Error "Non-fieldable value"
      )

  let at fields dec =
    List.fold_right field fields dec

  let index idx (Decoder decoder) =
    Decoder
      ( fun value ->
          let open Web.Json in
          match classify value with
          | JSONArray a ->
            if idx < 0 || idx > (Array.length a)
            then Tea_result.Error ("Array index out of range: " ^ (string_of_int idx))
            else decoder a.(idx)
          | _ -> Tea_result.Error "Non-array value"
      )

  let maybe (Decoder decoder) =
    Decoder
      ( fun value ->
          match decoder value with
          | Tea_result.Ok r -> Tea_result.Ok (Some r)
          | Tea_result.Error _ -> Tea_result.Ok None
      )

  let oneOf decoders =
    Decoder
      ( fun value ->
          let rec parse v = function
            | [] -> Tea_result.Error "No one-of's matched"
            | ((Decoder decoder) :: rest) ->
              try
                match decoder v with
                | Tea_result.Ok _ as ok -> ok
                | Tea_result.Error _ -> parse v rest
              with _ -> parse v rest
          in parse value decoders
      )

  let map mapper
      (Decoder decoder1) =
    Decoder
      ( fun value ->
          let open Tea_result in
          match decoder1 value with
          | Ok v1 -> Ok (mapper v1)
          | Error e -> Error ("map " ^ e)
      )

  let map2 mapper
    (Decoder decoder1)
    (Decoder decoder2) =
  Decoder
    ( fun value ->
        let open Tea_result in
        match
          decoder1 value,
          decoder2 value
        with
        | Ok v1,
          Ok v2 -> Ok (mapper v1 v2)
        | e1, e2 ->
          match Tea_result.error_of_first e1 e2 with
          | None -> failwith "Impossible case"
          | Some e -> Error ("map2 -> " ^ e)
    )

  let map3 mapper
    (Decoder decoder1)
    (Decoder decoder2)
    (Decoder decoder3) =
  Decoder
    ( fun value ->
        let open Tea_result in
        match
          decoder1 value,
          decoder2 value,
          decoder3 value
        with
        | Ok v1,
          Ok v2,
          Ok v3 -> Ok (mapper v1 v2 v3)
        | e1, e2, e3 ->
          let open! Tea_result in
          match
            e1
            |> first e2
            |> first e3
          with
          | Ok _ -> failwith "Impossible case"
          | Error e -> Error ("map3 -> " ^ e)
    )

  let map4 mapper
    (Decoder decoder1)
    (Decoder decoder2)
    (Decoder decoder3)
    (Decoder decoder4) =
  Decoder
    ( fun value ->
        let open Tea_result in
        match
          decoder1 value,
          decoder2 value,
          decoder3 value,
          decoder4 value
        with
        | Ok v1,
          Ok v2,
          Ok v3,
          Ok v4 -> Ok (mapper v1 v2 v3 v4)
        | e1, e2, e3, e4 ->
          let open! Tea_result in
          match
            e1
            |> first e2
            |> first e3
            |> first e4
          with
          | Ok _ -> failwith "Impossible case"
          | Error e -> Error ("map4 -> " ^ e)
    )

  let map5 mapper
    (Decoder decoder1)
    (Decoder decoder2)
    (Decoder decoder3)
    (Decoder decoder4)
    (Decoder decoder5) =
  Decoder
    ( fun value ->
        let open Tea_result in
        match
          decoder1 value,
          decoder2 value,
          decoder3 value,
          decoder4 value,
          decoder5 value
        with
        | Ok v1,
          Ok v2,
          Ok v3,
          Ok v4,
          Ok v5 -> Ok (mapper v1 v2 v3 v4 v5)
        | e1, e2, e3, e4, e5 ->
          let open! Tea_result in
          match
            e1
            |> first e2
            |> first e3
            |> first e4
            |> first e5
          with
          | Ok _ -> failwith "Impossible case"
          | Error e -> Error ("map5 -> " ^ e)
    )

  let map6 mapper
    (Decoder decoder1)
    (Decoder decoder2)
    (Decoder decoder3)
    (Decoder decoder4)
    (Decoder decoder5)
    (Decoder decoder6) =
  Decoder
    ( fun value ->
        let open Tea_result in
        match
          decoder1 value,
          decoder2 value,
          decoder3 value,
          decoder4 value,
          decoder5 value,
          decoder6 value
        with
        | Ok v1,
          Ok v2,
          Ok v3,
          Ok v4,
          Ok v5,
          Ok v6 -> Ok (mapper v1 v2 v3 v4 v5 v6)
        | e1, e2, e3, e4, e5, e6 ->
          let open! Tea_result in
          match
            e1
            |> first e2
            |> first e3
            |> first e4
            |> first e5
            |> first e6
          with
          | Ok _ -> failwith "Impossible case"
          | Error e -> Error ("map6 -> " ^ e)
    )

  let map7 mapper
    (Decoder decoder1)
    (Decoder decoder2)
    (Decoder decoder3)
    (Decoder decoder4)
    (Decoder decoder5)
    (Decoder decoder6)
    (Decoder decoder7) =
  Decoder
    ( fun value ->
        let open Tea_result in
        match
          decoder1 value,
          decoder2 value,
          decoder3 value,
          decoder4 value,
          decoder5 value,
          decoder6 value,
          decoder7 value
        with
        | Ok v1,
          Ok v2,
          Ok v3,
          Ok v4,
          Ok v5,
          Ok v6,
          Ok v7 -> Ok (mapper v1 v2 v3 v4 v5 v6 v7)
        | e1, e2, e3, e4, e5, e6, e7 ->
          let open! Tea_result in
          match
            e1
            |> first e2
            |> first e3
            |> first e4
            |> first e5
            |> first e6
            |> first e7
          with
          | Ok _ -> failwith "Impossible case"
          | Error e -> Error ("map7 -> " ^ e)
    )

  let map8 mapper
    (Decoder decoder1)
    (Decoder decoder2)
    (Decoder decoder3)
    (Decoder decoder4)
    (Decoder decoder5)
    (Decoder decoder6)
    (Decoder decoder7)
    (Decoder decoder8) =
  Decoder
    ( fun value ->
        let open Tea_result in
        match
          decoder1 value,
          decoder2 value,
          decoder3 value,
          decoder4 value,
          decoder5 value,
          decoder6 value,
          decoder7 value,
          decoder8 value
        with
        | Ok v1,
          Ok v2,
          Ok v3,
          Ok v4,
          Ok v5,
          Ok v6,
          Ok v7,
          Ok v8 -> Ok (mapper v1 v2 v3 v4 v5 v6 v7 v8)
        | e1, e2, e3, e4, e5, e6, e7, e8 ->
          let open! Tea_result in
          match
            e1
            |> first e2
            |> first e3
            |> first e4
            |> first e5
            |> first e6
            |> first e7
            |> first e8
          with
          | Ok _ -> failwith "Impossible case"
          | Error e -> Error ("map8 -> " ^ e)
    )


  (* Fancy Primitives *)

  let succeed v =
    Decoder
      ( fun _value ->
          Tea_result.Ok v
      )

  let fail e =
    Decoder
      ( fun _value ->
          Tea_result.Error e
      )

  let value =
    Decoder
      ( fun value ->
          Tea_result.Ok value
      )

  let andThen func (Decoder decoder) =
    Decoder
      ( fun value ->
          match decoder value with
          | Tea_result.Ok r ->
            let (Decoder andThenDecoder) = func r in
            andThenDecoder value
          | Tea_result.Error _ as err -> err
      )

  let lazy_ func =
    andThen func (succeed ())

  let nullable decoder =
    oneOf
      [ null None
      ; map (fun v -> Some v) decoder
      ]


  (* Decoders *)

  (* TODO:  Constrain this value type more *)
  let decodeValue (Decoder decoder) value =
    try decoder value
    with
    | ParseFail e -> Tea_result.Error e
    | _ -> Tea_result.Error "Unknown JSON parsing error"

  let decodeEvent (Decoder decoder) (value : Web_node.event) =
    try decoder (Obj.magic value)
    with
    | ParseFail e -> Tea_result.Error e
    | _ -> Tea_result.Error "Unknown JSON parsing error"

  let decodeString decoder string =
    try
      let value = Web.Json.parseExn string in
      decodeValue decoder value
    with
    (* | JsException e -> Tea_result.Error ("Given an invalid JSON: " ^ e) *)
    | _ -> Tea_result.Error "Invalid JSON string"

end


module Encoder = struct
  open Web

  type t = Json.t

  let encode indentLevel value =
    Web.Json.string_of_json ~indent:indentLevel (Js.Undefined.return value)


  (* Encoders *)

  let string (v : string) = Json.of_type Json.String v

  let int (v : int) = Json.of_type Json.Number (float_of_int v)

  let float (v : float) = Json.of_type Json.Number v

  let bool (v : bool) = Json.of_type Json.Boolean (if v then Js.true_ else Js.false_)

  let null = Json.of_type Json.Null Json.null

  let object_ v =
    let aux o (k, v) =
      let () = Js.Dict.set o k v in
      o in
    let o = List.fold_left aux (Js.Dict.empty ()) v in
    Json.of_type Json.Object o

  let array (v : 't array) = Json.of_type Json.Array v

  let list (v : 't list) = Json.of_type Json.Array (Array.of_list v)

end


type t = Web.Json.t
