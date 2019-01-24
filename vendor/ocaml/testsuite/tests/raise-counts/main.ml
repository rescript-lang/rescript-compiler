(* TEST
  modules = "a.ml b.ml"
*)

(* PR#7702 *)

let () =
  B.bug (Some "");
  print_endline "OK."
