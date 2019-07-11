external u : int = "u" [@@bs.val]
external vv : int = "vv" [@@bs.val] [@@bs.module "x"]
external vvv : int = "vv" [@@bs.val] [@@bs.module "x", "U"]
external vvvv : int = "vvvv" [@@bs.module "x", "U"]

(* TODO: unify all [bs.module] name, here ideally, we should have only one
   [require("x")] here *)
let h = u
let hh = vv
let hhh = vvv
let hhhh = vvvv
