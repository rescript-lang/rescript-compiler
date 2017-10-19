
type position = {
  x : int;
  y : int;
}


let position =
  let open Tea_json.Decoder in
  map2 (fun x y -> {x; y})
    (field "pageX" int)
    (field "pageY" int)


let registerGlobal name key tagger =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn = fun ev ->
      let open Tea_json.Decoder in
      let open Tea_result in
      match decodeEvent position ev with
      | Error _ -> None
      | Ok pos -> Some (tagger pos) in
    let handler = EventHandlerCallback (key, fn) in
    let elem = Web_node.document_node in
    let cache = eventHandler_Register callbacks elem name handler in
    fun () ->
      let _ = eventHandler_Unregister elem name cache in
      ()
  in Tea_sub.registration key enableCall

let clicks ?(key="") tagger =
  registerGlobal "click" key tagger

let moves ?(key="") tagger =
  registerGlobal "mousemove" key tagger

let downs ?(key="") tagger =
  registerGlobal "mousedown" key tagger

let ups ?(key="") tagger =
  registerGlobal "mouseup" key tagger
