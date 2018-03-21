
[@@@ocaml.warning "-a"]

type t = {

  mutable hi : int
    [@bs.as "Content-Type"];
  mutable low : int
    [@bs.as "l"];
  mutable x : int;
    [@bs.as "open"]
} [@@bs.deriving abstract]
  (* [@@bs.x] *)


let v = t ~hi:3 ~low:2 ~x:2


let (a,b,c) = (v |. hi, v |. low, v |. x)

(**

  v |. (hi, lo)
*)
let ff () =
  v |. hiSet 3;
  v |. lowSet 2
