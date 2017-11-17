type t

external getItem : string -> string option = "" [@@bs.send.pipe: t] [@@bs.return null_to_opt]
[@@deprecated "Use `get` instead"]
external get : string -> string option = "getItem" [@@bs.send.pipe: t] [@@bs.return null_to_opt]

external setItem : string -> string -> unit = "" [@@bs.send.pipe: t]
[@@deprecated "Use `set` instead"]
external set : string -> string -> unit = "setItem" [@@bs.send.pipe: t]

external removeItem : string -> unit = "" [@@bs.send.pipe: t]
[@@deprecated "Use `remove` instead"]
external remove : string -> unit = "removeItem" [@@bs.send.pipe: t]

external clear : unit = "" [@@bs.send.pipe: t]
external key : int -> string option = "" [@@bs.send.pipe: t] [@@bs.return null_to_opt]
external length : t -> int = "" [@@bs.get]

external localStorage : t = "" [@@bs.val]
[@@deprecated "Use `local` instead"]
external local : t = "localStorage" [@@bs.val]

external sessionStorage : t = "" [@@bs.val]
[@@deprecated "Use `session` instead"]
external session : t = "sessionStorage" [@@bs.val]