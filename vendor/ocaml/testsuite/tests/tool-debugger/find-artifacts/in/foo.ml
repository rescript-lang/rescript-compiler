open Blah

let print = function
  | Foo -> print_endline "Foo";
  | Bar s -> print_endline ("Bar(" ^ s ^ ")")

let main () =
  let x = Foo in
  let y = Bar "hi" in
  print x;
  print y

let _ = main ()
