type t = Dom_storage2.t

external getItem : string -> string option = "getItem" [@@bs.send.pipe: t] [@@bs.return null_to_opt]
external setItem : string -> string -> unit = "setItem" [@@bs.send.pipe: t]
external removeItem : string -> unit = "removeItem" [@@bs.send.pipe: t]
external clear : unit = "clear" [@@bs.send.pipe: t]
external key : int -> string option = "key" [@@bs.send.pipe: t] [@@bs.return null_to_opt]
external length : t -> int = "length" [@@bs.get]

external localStorage : t = "localStorage" [@@bs.val]
external sessionStorage : t = "sessionStorage" [@@bs.val]
