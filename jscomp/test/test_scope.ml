

external f : int -> int = "" [@@bs.call "x"] 
    (* [@@bs.scope "u"] [@@bs.scope "uuu"] *)
external ff : int -> int = "" [@@bs.call "x"] 
let h  = f 3
let hh = ff 3
let f x y = x ^ y
