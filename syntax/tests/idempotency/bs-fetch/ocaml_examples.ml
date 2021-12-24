let _ =
  Js.Promise.(
    Fetch.fetch "/api/hellos/1"
    |> then_ Fetch.Response.text
    |> then_ (fun text -> print_endline text |> resolve)
  )

let _ =
  Js.Promise.(
    Fetch.fetchWithInit "/api/hello" (Fetch.RequestInit.make ~method_:Post ())
    |> then_ Fetch.Response.text
    |> then_ (fun text -> print_endline text |> resolve)
  )

let _ =
  Js.Promise.(
    Fetch.fetch "/api/fruit"
    (* assume server returns `["apple", "banana", "pear", ...]` *)
    |> then_ Fetch.Response.json
    |> then_ (fun json -> Js.Json.decodeArray json |> resolve)
    |> then_ (fun opt -> Belt.Option.getExn opt |> resolve)
    |> then_ (fun items ->
        items |> Js.Array.map (fun item ->
                    item |> Js.Json.decodeString
                         |> Belt.Option.getExn)
              |> resolve)
  )

let _ =
  let payload = Js.Dict.empty () in
  Js.Dict.set payload "hello" (Js.Json.string "world");
  let open Js.Promise in
    (Fetch.fetchWithInit "/api/hello"
       (Fetch.RequestInit.make ~method_:Post
          ~body:(Fetch.BodyInit.make
                   (Js.Json.stringify (Js.Json.object_ payload)))
          ~headers:(Fetch.HeadersInit.makeWithDict (Js.Dict.fromList [
                     ("Content-Type", "application/json")])) ()))
      |> (then_ Fetch.Response.json)
