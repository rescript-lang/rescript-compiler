

external u: int = "u" [@@js.global]

external vv : int = "vv" [@@js.global] [@@js.module "x"]

external vvv : int = "vv" [@@js.global] [@@js.module "x" "U"]
external vvvv : int = "vvvv" [@@js.global] [@@js.module "x" "U"]
(* TODO: unify all [js.module] name, here ideally, 
   we should have only one [require("x")] here *)
let h = u 
let hh = vv  
let hhh = vvv
let hhhh = vvvv
