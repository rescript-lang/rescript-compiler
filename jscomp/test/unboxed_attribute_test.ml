

type t = A of int [@@ocaml.unboxed]

let v0 = A 3

let make x = A x 

let get (A x) = x