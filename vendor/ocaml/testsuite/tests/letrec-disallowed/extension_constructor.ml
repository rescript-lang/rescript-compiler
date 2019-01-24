(* Example from Stephen Dolan.
   Accessing an extension constructor involves accessing the module
   in which it's defined.
 *)
module type T =
  sig exception A of int end;;

let rec x =
  let module M = (val m) in
  M.A 42
and (m : (module T)) =
  (module (struct exception A of int end));;
