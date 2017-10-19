
type unresolved

type xmlHttpRequestUpload

type event_readystatechange = Web_json.t
type event_abort = Web_json.t
type event_error = Web_json.t
type event_load = Web_json.t
type event_loadstart = Web_json.t
type event_progress = Web_json.t
type event_timeout = Web_json.t
type event_loadend = Web_json.t

class type _xmlhttprequest = object
  (* Methods *)
  method abort : unit -> unit
  method getAllResponseHeaders : unit -> string Js.null
  method getResponseHeader : string -> string Js.null
  method _open : string -> string -> bool -> string -> string -> unit
  method overrideMimeType : string -> unit
  method send : unit -> unit
  method send__string : string Js.null -> unit
  method send__formdata : Web_formdata.t -> unit
  method send__document : Web_document.t -> unit
  (* method send_blob : Web_blob.t -> unit *)
  (* method send_arrayBufferView : Web_arraybuffer_view.t -> unit *)
  method setRequestHeader : string -> string -> unit

  (* Properties *)
  method onreadystatechange : (event_readystatechange -> unit) [@@bs.get] [@@bs.set]
  method readyState : int [@@bs.get]
  method responseType : string [@@bs.get] [@@bs.set]
  method response : unresolved Js.null [@@bs.get]
  method responseText : string [@@bs.get]
  method responseURL : string [@@bs.get]
  method responseXML : Web_document.t Js.null [@@bs.get]
  method status : int [@@bs.get]
  method statusText : string [@@bs.get]
  method timeout : float [@@bs.get] [@@bs.set]
  method upload : xmlHttpRequestUpload [@@bs.get]
  method withCredentials : bool [@@bs.get] [@@bs.set]

  (* Base events *)
  method onabort : (event_abort -> unit) [@@bs.get] [@@bs.set]
  method onerror : (event_error -> unit) [@@bs.get] [@@bs.set]
  method onload : (event_load -> unit) [@@bs.get] [@@bs.set]
  method onloadstart : (event_loadstart -> unit) [@@bs.get] [@@bs.set]
  method onprogress : (event_loadstart -> unit) [@@bs.get] [@@bs.set]
  method ontimeout : (event_timeout -> unit) [@@bs.get] [@@bs.set]
  method onloadend : (event_loadend -> unit) [@@bs.get] [@@bs.set]
end [@bs]
type t = _xmlhttprequest Js.t

external create : unit -> t = "XMLHttpRequest" [@@bs.new]

type errors =
  | IncompleteResponse
  | NetworkError

type body =
  | EmptyBody
  | EmptyStringBody
  | StringBody of string
  | FormDataBody of Web_formdata.t
  | FormListBody of (string * string) list
  | DocumentBody of Web_document.t
  (* | BlobBody of Web_blob.t *)
  (* | ArrayBufferViewBody of Web_arraybuffer_view.t *)

(* Main interface functions *)

let abort x = x##abort ()

let getAllResponseHeaders x =
  let open Tea_result in
  match Js.Null.to_opt (x##getAllResponseHeaders ()) with
  | None -> Error IncompleteResponse
  | Some "" -> Error NetworkError
  | Some s -> Ok s

let getAllResponseHeadersAsList x =
  let open Tea_result in
  match getAllResponseHeaders x with
  | Error _ as err -> err
  | Ok s -> Ok
    ( s
      |> Js.String.split "\r\n"
      |> Array.map (Js.String.splitAtMost ": " ~limit:2)
      |> Array.to_list
      |> List.filter (fun a -> Array.length a == 2)
      |> List.map
        ( function
          | [|key; value|] -> (key, value)
          | _ -> failwith "Cannot happen, already checked length"
        )
    )

let getAllResponseHeadersAsDict x =
  let module StringMap = Map.Make(String) in
  match getAllResponseHeadersAsList x with
  | Tea_result.Error _ as err -> err
  | Tea_result.Ok l ->
    let insert d (k, v) = StringMap.add k v d in
    Tea_result.Ok (List.fold_left insert StringMap.empty l)

let getResponseHeader key x = Js.Null.to_opt (x##getResponse key)

let open_ method' url ?(async=true) ?(user="") ?(password="") x =
  x##_open method' url async user password

let overrideMimeType mimetype x = x##overrideMimeType mimetype

let send body x =
  match body with
  | EmptyBody -> x##send ()
  | EmptyStringBody -> x##send__string Js.Null.empty
  | StringBody s -> x##send__string (Js.Null.return s)
  | FormDataBody f -> x##send__formdata f
  | FormListBody l ->
    let form =
      List.fold_left
        (fun f (key, value) -> let () = Web_formdata.append key value f in f)
        (Web_formdata.create ())
        l in
    x##send__formdata form
  | DocumentBody d -> x##send__document d
  (* | BlobBody b -> x##send_blob b *)
  (* | ArrayBufferViewBody a -> x##send_arrayBufferView a *)

let setRequestHeader header value x = x##setRequestHeader header value


(* Properties *)

type state =
  | Unsent
  | Opened
  | HeadersReceived
  | Loading
  | Done

type responseType =
  | StringResponseType
  | ArrayBufferResponseType
  | BlobResponseType
  | DocumentResponseType
  | JsonResponseType
  | TextResponseType
  | RawResponseType of string

type responseBody =
  | NoResponse
  | StringResponse of string
  | ArrayBufferResponse of unit
  | BlobResponse of unit
  | DocumentResponse of Web_document.t
  | JsonResponse of Web_json.t
  | TextResponse of string
  | RawResponse of string * unit

let set_onreadystatechange cb x = x##onreadystatechange #= cb

let get_onreadystatechange x = x##onreadystatechange

let readyState x =
  match x##readystate with
  | 0 -> Unsent
  | 1 -> Opened
  | 2 -> HeadersReceived
  | 3 -> Loading
  | 4 -> Done
  | i -> failwith ("Invalid return from 'readystate' of: " ^ string_of_int i)

let set_responseType typ x =
  match typ with
  | StringResponseType -> x##responseType #= ""
  | ArrayBufferResponseType -> x##responseType #= "arraybuffer"
  | BlobResponseType -> x##responseType #= "blob"
  | DocumentResponseType -> x##responseType #= "document"
  | JsonResponseType -> x##responseType #= "json"
  | TextResponseType -> x##responseType #= "text"
  | RawResponseType s -> x##responseType #= s

let get_responseType x =
  match x##responseType with
  | "" -> StringResponseType
  | "arraybuffer" -> ArrayBufferResponseType
  | "blob" -> BlobResponseType
  | "document" -> DocumentResponseType
  | "json" -> JsonResponseType
  | "text" -> TextResponseType
  | s -> RawResponseType s

let get_response x =
  match Js.Null.to_opt x##response with
  | None -> NoResponse
  | Some resp ->
    match get_responseType x with
    | StringResponseType -> StringResponse (Obj.magic resp)
    | ArrayBufferResponseType -> ArrayBufferResponse (Obj.magic resp)
    | BlobResponseType -> BlobResponse (Obj.magic resp)
    | DocumentResponseType -> DocumentResponse (Obj.magic resp)
    | JsonResponseType -> JsonResponse (Obj.magic resp)
    | TextResponseType -> TextResponse (Obj.magic resp)
    | RawResponseType s -> RawResponse (s, Obj.magic resp)

let get_responseText x = x##responseText

let get_responseURL x = x##responseURL

let get_responseXML x = Js.Null.to_opt x##responseXML

let get_status x = x##status

let get_statusText x = x##statusText

let set_timeout t x = x##timeout #= t

let get_timeout x = x##timeout

let set_withCredentials b x = x##withCredentials #= b

let get_withCredentials x = x##withCredentials

let set_onabort cb x = x##onabort #= cb

let get_onabort x = x##onabort

let set_onerror cb x = x##onerror #= cb

let get_onerror x = x##onerror

let set_onload cb x = x##onload #= cb

let get_onload x = x##onload

let set_onloadstart cb x = x##onloadstart #= cb

let get_onloadstart x = x##onloadstart

let set_onprogress cb x = x##onprogress #= cb

let get_onprogress x = x##onprogress

let set_ontimeout cb x = x##ontimeout #= cb

let get_ontimeout x = x##ontimeout

let set_onloadend cb x = x##onloadend #= cb

let get_onloadend x = x##onloadend
