type t

external getItem : t -> string -> string option = "" [@@bs.send] [@@bs.return null_to_opt]
external setItem : t -> string -> string -> unit = "" [@@bs.send]
external removeItem : t -> string -> unit = "" [@@bs.send]
external clear : t -> unit = "" [@@bs.send]
external key : t -> int -> string option = "" [@@bs.send] [@@bs.return null_to_opt]
external length : t -> int = "" [@@bs.get]

external localStorage : t = "" [@@bs.val]
external sessionStorage : t = "" [@@bs.val]
