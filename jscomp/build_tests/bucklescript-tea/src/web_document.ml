
(* TODO:  Polyfill document if it is missing, like on node or in native *)

type t = <
  body : Web_node.t [@bs.get];
  createElement : string -> Web_node.t [@bs.meth];
  createElementNS : string -> string -> Web_node.t [@bs.meth];
  createComment : string -> Web_node.t [@bs.meth];
  createTextNode : string -> Web_node.t [@bs.meth];
  getElementById : string -> Web_node.t Js.null_undefined [@bs.meth];
  location : Web_location.t [@bs.get];
> Js.t

external document : t = "document" [@@bs.val]

let body () = document##body

let createElement typ = document##createElement typ

let createElementNS namespace key = document##createElementNS namespace key

let createComment text = document##createComment text

let createTextNode text = document##createTextNode text

let getElementById id = document##getElementById id

let createElementNsOptional namespace tagName =
  match namespace with
  | "" -> document##createElement tagName
  | ns -> document##createElementNS ns tagName

let location () = document##location
