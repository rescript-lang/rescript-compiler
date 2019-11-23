
(*
To work around unused attribute checking

- we mark it used in ppx stage
we can not mark it in parsing since it won't
works for reason
*)
type t = {
  mutable x : int [@bs.as "THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE"]
   (* test key word later *)
}



let f x  = { x}

let set x = x.x <- 3 ; x.x * 2

type x = t = private {
  mutable x : int [@bs.as "THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE"]
}

type t0 = { x: t0 ; y : int}

let f2 u = 
match u with 
| {x = { x = {x={y}}}}   ->     y


