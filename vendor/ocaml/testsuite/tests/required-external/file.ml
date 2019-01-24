external getcwd : unit -> string = "caml_sys_getcwd"

let f () = ()

let () =
  print_endline "Module `File' is linked";
  flush stdout
