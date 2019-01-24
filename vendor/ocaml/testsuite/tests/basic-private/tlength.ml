(* TEST
  modules = "length.ml"
*)

(*

A testbed file for private type abbreviation definitions.

We test the Length module that implements positive integers.

*)

(* We can build a null length. *)
let l = Length.make 0;;


(* We cannot build a negative length. *)
try ignore (Length.make (-1)); assert false with
| Failure _ -> ()
;;


(* We can build a positive length. *)
let l3 = Length.make 3 in

(* and use the associated injection and projection functions. *)
Length.make (Length.from l3 + Length.from l3);;
