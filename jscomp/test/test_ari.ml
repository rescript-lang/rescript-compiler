let f x = (+) x 
(** f = function (x){ function(x,y){x + y} (x) }*)
let f1 x y  = (+) x y 
let f3 g x = g x 
let f2 = (+) 3 
let g = f 3 4 


external ext  : int -> int -> int = "test_primit"[@@js.call "test_primit"] [@@js.module "U"]

let ff = ext 3 
type u = int -> int 
external ext  : int -> u  = "test_primit2" [@@js.call "test_primit2"][@@js.module "VV"]
let fff = ext 3  

let rec length_aux len = function
    [] -> len
  | a::l -> length_aux (len + 1) l

include List 
