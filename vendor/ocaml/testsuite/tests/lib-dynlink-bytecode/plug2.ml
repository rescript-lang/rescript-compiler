external stub2: unit -> unit = "stub2"

let f x = print_string "This is Plug2.f\n"; x + 2

let () = Registry.register f

let () = stub2 ()
