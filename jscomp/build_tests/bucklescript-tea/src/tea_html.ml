open Vdom

module Cmds = Tea_html_cmds

(* let map lift vdom =
   *)

(* Nodes *)

let noNode = noNode

let text str = text str

let lazy1 key gen = lazyGen key gen

let node ?(namespace="") tagName ?(key="") ?(unique="") props nodes = fullnode namespace tagName key unique props nodes

(* let embedProgram main = custom *)


(* HTML Elements *)

let br props = fullnode "" "br" "br" "br" props []

let br' ?(key="") ?(unique="") props nodes = fullnode "" "br" key unique props nodes

let div ?(key="") ?(unique="") props nodes = fullnode "" "div" key unique props nodes

let span ?(key="") ?(unique="") props nodes = fullnode "" "span" key unique props nodes

let p ?(key="") ?(unique="") props nodes = fullnode "" "p" key unique props nodes

let pre ?(key="") ?(unique="") props nodes = fullnode "" "pre" key unique props nodes

let a ?(key="") ?(unique="") props nodes = fullnode "" "a" key unique props nodes

let section ?(key="") ?(unique="") props nodes = fullnode "" "section" key unique props nodes

let header ?(key="") ?(unique="") props nodes = fullnode "" "header" key unique props nodes

let footer ?(key="") ?(unique="") props nodes = fullnode "" "footer" key unique props nodes

let h1 ?(key="") ?(unique="") props nodes = fullnode "" "h1" key unique props nodes

let h2 ?(key="") ?(unique="") props nodes = fullnode "" "h2" key unique props nodes

let h3 ?(key="") ?(unique="") props nodes = fullnode "" "h3" key unique props nodes

let h4 ?(key="") ?(unique="") props nodes = fullnode "" "h4" key unique props nodes

let h5 ?(key="") ?(unique="") props nodes = fullnode "" "h5" key unique props nodes

let h6 ?(key="") ?(unique="") props nodes = fullnode "" "h6" key unique props nodes

let i ?(key="") ?(unique="") props nodes = fullnode "" "i" key unique props nodes

let strong ?(key="") ?(unique="") props nodes = fullnode "" "strong" key unique props nodes

let button ?(key="") ?(unique="") props nodes = fullnode "" "button" key unique props nodes

let input' ?(key="") ?(unique="") props nodes = fullnode "" "input" key unique props nodes

let textarea ?(key="") ?(unique="") props nodes = fullnode "" "textarea" key unique props nodes

let label ?(key="") ?(unique="") props nodes = fullnode "" "label" key unique props nodes

let ul ?(key="") ?(unique="") props nodes = fullnode "" "ul" key unique props nodes

let ol ?(key="") ?(unique="") props nodes = fullnode "" "ol" key unique props nodes

let li ?(key="") ?(unique="") props nodes = fullnode "" "li" key unique props nodes

let table ?(key="") ?(unique="") props nodes = fullnode "" "table" key unique props nodes

let thead ?(key="") ?(unique="") props nodes = fullnode "" "thead" key unique props nodes

let tfoot ?(key="") ?(unique="") props nodes = fullnode "" "tfoot" key unique props nodes

let tbody ?(key="") ?(unique="") props nodes = fullnode "" "tbody" key unique props nodes

let th ?(key="") ?(unique="") props nodes = fullnode "" "th" key unique props nodes

let tr ?(key="") ?(unique="") props nodes = fullnode "" "tr" key unique props nodes

let td ?(key="") ?(unique="") props nodes = fullnode "" "td" key unique props nodes

let progress ?(key="") ?(unique="") props nodes = fullnode "" "progress" key unique props nodes

let img ?(key="") ?(unique="") props nodes = fullnode "" "img" key unique props nodes

let select ?(key="") ?(unique="") props nodes = fullnode "" "select" key unique props nodes

let option' ?(key="") ?(unique="") props nodes = fullnode "" "option" key unique props nodes

let form ?(key="") ?(unique="") props nodes = fullnode "" "form" key unique props nodes

let nav ?(key="") ?(unique="") props nodes = fullnode "" "nav" key unique props nodes

let main ?(key="") ?(unique="") props nodes = fullnode "" "main" key unique props nodes

let aside ?(key="") ?(unique="") props nodes = fullnode "" "aside" key unique props nodes

let article ?(key="") ?(unique="") props nodes = fullnode "" "article" key unique props nodes

let details ?(key="") ?(unique="") props nodes = fullnode "" "details" key unique props nodes

let figcaption ?(key="") ?(unique="") props nodes = fullnode "" "figcaption" key unique props nodes

let figure ?(key="") ?(unique="") props nodes = fullnode "" "figure" key unique props nodes

let mark ?(key="") ?(unique="") props nodes = fullnode "" "mark" key unique props nodes

let summary ?(key="") ?(unique="") props nodes = fullnode "" "summary" key unique props nodes

let time ?(key="") ?(unique="") props nodes = fullnode "" "time" key unique props nodes

(* Properties *)

let noProp = Vdom.noProp

let id str = prop "id" str

(* `href` is actually an attribute, not a property, but need it here for Elm compat... *)
let href str = attribute "" "href" str

(* `src` is actually an attribute, not a property, but need it here for Elm compat... *)
let src str = attribute "" "src" str

let class' name = prop "className" name

let classList classes =
  classes
  |> List.filter (fun (_fst, snd) -> snd)
  |> List.map (fun (fst, _snd) -> fst)
  |> String.concat " "
  |> class'

let type' typ = prop "type" typ

let style key value = style key value

let styles s = styles s

let placeholder str = prop "placeholder" str

let autofocus b = if b then prop "autofocus" "autofocus" else noProp

let value str = prop "value" str

let name str = prop "name" str

let checked b = if b then prop "checked" "checked" else noProp

let for' str = prop "htmlFor" str

let hidden b = if b then prop "hidden" "hidden" else noProp

let target t = prop "target" t

let action a = prop "action" a

let method' m = prop "method" m

(* Events *)

let onCB eventName key cb = onCB eventName key cb

let onMsg eventName msg = onMsg eventName msg

let onInputOpt ?(key="") msg =
  onCB "input" key
    (fun ev ->
       match Js.Undefined.to_opt ev##target with
       | None -> None
       | Some target -> match Js.Undefined.to_opt target##value with
         | None -> None
         | Some value -> msg value
    )

let onInput ?(key="") msg = onInputOpt ~key:key (fun ev -> Some (msg ev))

let onChangeOpt ?(key="") msg =
  onCB "change" key
  (fun ev ->
       match Js.Undefined.to_opt ev##target with
       | None -> None
       | Some target -> match Js.Undefined.to_opt target##value with
         | None -> None
         | Some value -> msg value
    )

let onChange ?(key="") msg = onChangeOpt ~key:key (fun ev -> Some (msg ev))

let onClick msg =
  onMsg "click" msg

let onDoubleClick msg =
  onMsg "dblclick" msg

let onBlur msg =
  onMsg "blur" msg

let onFocus msg =
  onMsg "focus" msg

let onCheckOpt ?(key="") msg =
  onCB "change" key
    (fun ev ->
       match Js.Undefined.to_opt ev##target with
       | None -> None
       | Some target -> match Js.Undefined.to_opt target##checked with
         | None -> None
         | Some value -> msg value
    )

let onCheck ?(key="") msg = onCheckOpt ~key:key (fun ev -> Some (msg ev))

let onMouseDown msg =
  onMsg "mousedown" msg

let onMouseUp msg =
  onMsg "mouseup" msg

let onMouseEnter msg =
  onMsg "mouseenter" msg

let onMouseLeave msg =
  onMsg "mouseleave" msg

let onMouseOver msg =
  onMsg "mouseover" msg

let onMouseOut msg =
  onMsg "mouseout" msg


module Attributes = struct

  let max value = attribute "" "max" value

  let min value = attribute "" "min" value

  let step value = attribute "" "step" value

  let disabled b = if b then attribute "" "disabled" "true" else noProp

  let selected b = if b then attribute "" "selected" "true" else noProp

  let acceptCharset c = attribute "" "accept-charset" c

  let rel value = attribute "" "rel" value
end
