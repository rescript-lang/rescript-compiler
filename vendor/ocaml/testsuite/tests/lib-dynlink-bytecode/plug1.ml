external stub1: unit -> string = "stub1"

let f x = print_string "This is Plug1.f\n"; x + 1

let () = Registry.register f

let () = print_endline (stub1 ())
