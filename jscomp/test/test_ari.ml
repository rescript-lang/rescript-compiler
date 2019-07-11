let f x = ( + ) x

(* {[ f = function (x){ function(x,y){x + y} (x) } ]} after optimization {[ f =
   \x -> \y -> .. ]} actually will become {[ f = \(x,y) -> x + y ]} *)
let f1 x y = x + y
let f3 g x = g x
let f2 = ( + ) 3
let g = f 3 4

external ext : int -> int -> int = "test_primit"
  [@@bs.val "test_primit"] [@@bs.module "U"]

let ff = ext 3

type u = int -> int

external ext : int -> u = "test_primit2"
  [@@bs.val "test_primit2"] [@@bs.module "VV"]

let fff = ext 3

let rec length_aux len = function
  | [] -> len
  | a :: l -> length_aux (len + 1) l

include List
