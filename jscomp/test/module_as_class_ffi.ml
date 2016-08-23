
type t

external mk : int -> t = "xx/foo_class" [@@bs.new] [@@bs.module]

let f () =
  mk 3  
