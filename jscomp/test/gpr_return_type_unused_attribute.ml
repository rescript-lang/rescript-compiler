(* [@@@ocaml.warning "-101"] *)

external mk : int -> [`a | `b] (* [@bs.string] *) = "mk" [@@bs.val]

let v = mk 2

(* let h () = v = "x" *)
