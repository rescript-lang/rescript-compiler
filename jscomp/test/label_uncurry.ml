

open Js.Fn

type t = x:int -> y:string -> int [@bs]

type u =  (x:int -> y:string -> int) arity2

let f (x : t) : u = x


(* let u  f = f ~x:2 ~y:"x" [@bs] *)
