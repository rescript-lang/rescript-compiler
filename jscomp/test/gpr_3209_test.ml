(* [@@@ocaml.warning "a-11-8-4-a"] *)
[@@@ocaml.warning "a+11+8-8"]

type t6 =
  | T60
  | T61
  | T62
  | T63
  | T64 of int
  | T65 of int
  | T66 of int
  | T68 of int

let f9 = function T60 | T61 | T62 -> 1 | T64 _ | T65 _ -> 2 | _ -> 3
