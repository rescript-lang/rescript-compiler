[@@@ocaml.warning "-21-5"]

let foo g () = g 1; ()
let f1 ?x y = print_endline "f1"
let f2 ?x y = print_endline "f2"

let () =
  try foo (raise Exit; f1); print_endline "FAIL"
  with Exit -> print_endline "OK"

let r : (?x:unit -> int -> unit) ref = ref f1
let h = foo r.contents
let () = h (); r := f2; h ()
