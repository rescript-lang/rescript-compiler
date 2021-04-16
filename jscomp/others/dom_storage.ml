type t = Dom_storage2.t

external getItem : t -> string -> string option = "getItem" [@@send] [@@bs.return null_to_opt]
let getItem s obj = obj |. getItem s 
(* https://developer.mozilla.org/en-US/docs/Web/API/Storage/getItem 
  If the key does not exist, `null` is returned
*)

external setItem : t -> string -> string -> unit = "setItem" [@@send]
let setItem k v obj : unit = obj |. setItem k v 
external removeItem : t -> string -> unit = "removeItem" [@@send]
let removeItem s obj : unit = obj |. removeItem s 
external clear : t -> unit = "clear" [@@send]
external key : t -> int -> string option = "key" [@@send] [@@bs.return null_to_opt]
(* A DOMString containing the name of the key. If the index does not exist, null is returned.
  If the key does not exist, `null` is returned
*)
let key i obj : string option = obj |. key i 
external length : t -> int = "length" [@@get]

external localStorage : t = "localStorage" [@@val]
external sessionStorage : t = "sessionStorage" [@@val]
