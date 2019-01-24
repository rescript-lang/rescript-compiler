[@@@ocaml.warning "+4"]

type expr = E of int [@@unboxed]

      
let f x = match x with (E e) -> e

type t = A | B

let g x = match x with
| A -> 0
| _ -> 1
