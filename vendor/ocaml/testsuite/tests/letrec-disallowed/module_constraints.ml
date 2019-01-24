module type S = sig               val y : float end;;
module type T = sig val x : float val y : float end;;
type t = T : (module S) -> t;;

let rec x = let module M = (val m) in T (module M)
and (m : (module T)) = (module (struct let x = 10.0 and y = 20.0 end));;
