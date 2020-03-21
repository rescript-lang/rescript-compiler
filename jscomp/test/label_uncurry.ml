

open Js.Fn

type t = x:int -> y:string -> int [@bs]

type u =  (x:int -> y:string -> int) arity2

let f (x : t) : u = x

let u : u = fun [@bs] ~x ~y -> x + int_of_string y 

let u1  (f : u) = 
  (f  ~y:"x" ~x:2  [@bs]) |. Js.log ;
  (f  ~x:2 ~y:"x"   [@bs]) |. Js.log 
let h = fun [@bs] ~x:unit -> 3

let a = u1 u

(* let u1 (f : u) =
  Js.Internal.unsafeInvariantApply ((Js.Internal.run2 (f : u)) ~y:"x" ~x:2) 
  *)


type u0 = ?x:int -> y : string -> int [@bs]

(*let f = fun[@bs] ?x y -> x + y *)

(* let h (x :u0) = x ~y:"x" ~x:3 [@bs] *)