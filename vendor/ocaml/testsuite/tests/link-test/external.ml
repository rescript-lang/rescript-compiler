let () = print_endline "linked external"; flush stdout
external frexp : float -> float * int = "caml_frexp_float"
