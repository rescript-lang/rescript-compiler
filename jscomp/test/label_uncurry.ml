

open Js.Fn

type t = x:int -> y:string -> int [@bs]

type u =  (x:int -> y:string -> int) arity2

let f (x : t) : u = x

let u : u = fun [@bs] ~x ~y -> x + int_of_string y 

(* let u  f = f ~x:2 ~y:"x" [@bs] *)
let h = fun [@bs] ~x:unit -> 3