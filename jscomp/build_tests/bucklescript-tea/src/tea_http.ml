

type response_status =
  { code : int
  ; message : string
  }

type requestBody = Web.XMLHttpRequest.body
type bodyType = Web.XMLHttpRequest.responseType
type responseBody = Web.XMLHttpRequest.responseBody

type response =
  { url : string
  ; status : response_status
  ; headers : string Map.Make(String).t
  ; body : responseBody
  }

type 'parsedata error =
  | BadUrl of string
  | Timeout
  | NetworkError
  | Aborted
  | BadStatus of response
  | BadPayload of 'parsedata * response

let string_of_error = function
  | BadUrl url -> "Bad Url: " ^ url
  | Timeout -> "Timeout"
  | NetworkError -> "Unknown network error"
  | Aborted -> "Request aborted"
  | BadStatus resp -> "Bad Status: " ^ resp.url
  | BadPayload (_customData, resp) -> "Bad Payload: " ^ resp.url

type header = Header of string * string

type 'res expect =
    Expect of bodyType * (response -> ('res, string) Tea_result.t)

type 'msg requestEvents =
  { onreadystatechange : ('msg Vdom.applicationCallbacks ref -> (Web.XMLHttpRequest.event_readystatechange -> unit)) option
  ; onprogress : ('msg Vdom.applicationCallbacks ref -> (Web.XMLHttpRequest.event_progress -> unit)) option
  }

let emptyRequestEvents =
  { onreadystatechange = None
  ; onprogress = None
  }

type 'res rawRequest =
  { method' : string
  ; headers : header list
  ; url : string
  ; body : requestBody
  ; expect : 'res expect
  ; timeout : Tea_time.t option
  ; withCredentials : bool
  }

type ('msg, 'res) request =
    Request of 'res rawRequest * 'msg requestEvents option

let expectStringResponse func =
  let open Web.XMLHttpRequest in
  Expect
    ( TextResponseType
    , ( fun { body; _ } ->
          match body with
          | TextResponse s -> func s
          | _ -> Tea_result.Error "Non-text response returned"
      )
    )


let expectString =
  expectStringResponse (fun resString -> Tea_result.Ok resString)


let request rawRequest =
  Request (rawRequest, None)


let getString url =
  request
    { method' = "GET"
    ; headers = []
    ; url = url
    ; body = Web.XMLHttpRequest.EmptyBody
    ; expect = expectString
    ; timeout = None
    ; withCredentials = false
    }


let send resultToMessage (Request (request, maybeEvents)) =
  let module StringMap = Map.Make(String) in
  let {method'; headers; url; body; expect; timeout; withCredentials } = request in
  let (Expect (typ, responseToResult)) = expect in
  Tea_cmd.call (fun callbacks ->
      let enqRes result =
        fun _ev ->
          let open Vdom in
          !callbacks.enqueue (resultToMessage result) in
      let enqResError result = enqRes (Tea_result.Error result) in
      let enqResOk result = enqRes (Tea_result.Ok result) in
      let xhr = Web.XMLHttpRequest.create () in
      let setEvent ev cb = ev cb xhr in
      let () = match maybeEvents with
        | None -> ()
        | Some { onprogress; onreadystatechange } ->
          let open Web.XMLHttpRequest in
          let mayCB thenDo = function
            | None -> ()
            | Some v -> thenDo (v callbacks) in
          let () = mayCB (setEvent set_onreadystatechange) onreadystatechange in
          let () = mayCB (setEvent set_onprogress) onprogress in
          () in
      let () = setEvent Web.XMLHttpRequest.set_onerror (enqResError NetworkError) in
      let () = setEvent Web.XMLHttpRequest.set_ontimeout (enqResError Timeout) in
      let () = setEvent Web.XMLHttpRequest.set_onabort (enqResError Aborted) in
      let () = setEvent Web.XMLHttpRequest.set_onload
          ( fun _ev ->
              let open Web.XMLHttpRequest in
              let headers =
                match getAllResponseHeadersAsDict xhr with
                | Tea_result.Error _e -> StringMap.empty
                | Tea_result.Ok headers -> headers in
              let response =
                { status = { code = get_status xhr; message = get_statusText xhr }
                ; headers = headers
                ; url = get_responseURL xhr
                ; body = get_response xhr
                } in
              if response.status.code < 200 || 300 <= response.status.code
              then enqResError (BadStatus response) ()
              else match responseToResult response with
                | Tea_result.Error error -> enqResError (BadPayload (error, response)) ()
                | Tea_result.Ok result -> enqResOk result ()
          ) in
      let () = try Web.XMLHttpRequest.open_ method' url xhr
        with _ -> enqResError (BadUrl url) () in
      let () =
        let setHeader (Header (k, v)) = Web.XMLHttpRequest.setRequestHeader k v xhr in
        let () = List.iter setHeader headers in
        let () = Web.XMLHttpRequest.set_responseType typ xhr in
        let () =
          match timeout with
          | None -> ()
          | Some t -> Web.XMLHttpRequest.set_timeout t xhr in
        let () = Web.XMLHttpRequest.set_withCredentials withCredentials xhr in
        () in
      let () = Web.XMLHttpRequest.send body xhr in
      ()
    )

external encodeURIComponent : string -> string = "" [@@bs.val]

let encodeUri str =
    encodeURIComponent str

external decodeURIComponent : string -> string = "" [@@bs.val]

let decodeUri str =
    try Some (decodeURIComponent str)
        with _ -> None

module Progress = struct

  (*
  type bytesProgressed =
    { bytes : int
    ; bytesExpected : int
    }

  type ('data, 'parseFailData) t =
    | NoProgress
    (* SomeProgress (bytes, bytesExpected) *)
    | SomeProgress of bytesProgressed
    | FailProgress of 'parseFailData error
    | DoneProgress of 'data

  type ('msg, 'parseFailData) trackedRequest =
    { request : 'msg rawRequest
    ; toProgress : bytesProgressed -> 'msg
    ; toError : 'parseFailData error -> 'msg
    }
  *)

  type t =
    { bytes : int
    ; bytesExpected : int
    }

  let emptyProgress =
    { bytes = 0
    ; bytesExpected = 0
    }

  (* Yeah this does not follow the original API, but that original
     API is... not extensible...  Instead, we have generic event
     listener support here so no need to constrain the API.
     Might still want to make a subscription variant though... *)
  let track toMessage (Request (request, events)) =
    let onprogress = Some
        ( fun callbacks ev ->
            let open Vdom in
            let lengthComputable =
              let open Tea_json.Decoder in
              let open Tea_result in
              match decodeValue (field "lengthComputable" bool) ev with
              | Error _e -> false
              | Ok v -> v in
            if lengthComputable then
              let open Tea_json.Decoder in
              let open Tea_result in
              let decoder =
                map2 (fun bytes bytesExpected -> {bytes; bytesExpected})
                  (field "loaded" int)
                  (field "total" int)
              in
              match decodeValue decoder ev with
              | Error _e -> ()
              | Ok t ->
                !callbacks.enqueue (toMessage t)
        ) in
    let events =
      match events with
      | None -> emptyRequestEvents
      | Some e -> e
    in Request
      (request
      , Some { events with onprogress }
      )

end
