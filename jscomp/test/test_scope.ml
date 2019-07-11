external f : int -> int = "" [@@bs.val "x"]

(* [@@bs.scope "u"] [@@bs.scope "uuu"] *)
external ff : int -> int = "" [@@bs.val "x"]

let h = f 3
let hh = ff 3
let f x y = x ^ y
