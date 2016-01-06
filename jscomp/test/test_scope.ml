external f : int -> int = "" [@@js.call "x"] [@@js.scope "u"] [@@js.scope "uuu"]
external ff : int -> int = "" [@@js.call "x"] 
let h  = f 3
let hh = ff 3
let f x y = x ^ y
