

external u: int = "u" [@@js.val]

external vv : int = "vv" [@@js.val] [@@js.module "x"]

external vvv : int = "vv" [@@js.val] [@@js.module "x" "U"]
external vvvv : int = "vvvv" [@@js.val] [@@js.module "x" "U"]

(* TODO: unify all [js.module] name, here ideally, 
   we should have only one [require("x")] here *)
let h = u 
let hh = vv  
let hhh = vvv
let hhhh = vvvv
