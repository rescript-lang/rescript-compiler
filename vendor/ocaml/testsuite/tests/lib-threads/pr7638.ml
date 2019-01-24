(* MPR#7638 repro case *)

let crashme v =
  match Sys.getenv v with
  | exception Not_found -> print_string "OK\n"
  | s -> print_string "Surprising but OK\n"

let _ =
  let th = Thread.create crashme "no such variable" in
  Thread.join th
