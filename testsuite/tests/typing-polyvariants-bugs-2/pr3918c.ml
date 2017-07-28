(*
  ocamlc -c pr3918a.mli pr3918b.mli
  rm -f pr3918a.cmi
  ocamlc -c pr3918c.ml
*)

open Pr3918b

let f x = (x : 'a vlist :> 'b vlist)
let f (x : 'a vlist) = (x : 'b vlist)
