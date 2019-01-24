(* PR#5115 - multiple evaluation of bigarray expr *)

open Bigarray

let f y0 =
  Printf.printf "***EXEC***\n%!";
  y0

let _ =
  let y = Array1.of_array float64 fortran_layout [| 1. |] in
  ignore ((f y).{1});
  (f y).{1} <- 3.14
