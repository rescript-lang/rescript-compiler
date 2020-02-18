type t

external getItem : t -> string -> string option = "getItem" [@@bs.send] [@@bs.return null_to_opt]
external setItem : t -> string -> string -> unit = "setItem" [@@bs.send]
external removeItem : t -> string -> unit = "removeItem" [@@bs.send]
external clear : t -> unit = "clear" [@@bs.send]
external key : t -> int -> string option = "key" [@@bs.send] [@@bs.return null_to_opt]
external length : t -> int = "length" [@@bs.get]

external localStorage : t = "localStorage" [@@bs.val]
external sessionStorage : t = "sessionStorage" [@@bs.val]
