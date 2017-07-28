open A
let f =
  L.map S.capitalize

let () =
  L.iter print_endline (f ["jacques"; "garrigue"])

module C : sig module L : module type of List end = struct include A end

(* The following introduces a (useless) dependency on A:
module C : sig module L : module type of List end = A
*)

include D'
(*
let () =
  print_endline (string_of_int D'.M.y)
*)
