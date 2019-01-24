let () = print_endline "linked external from pack"; flush stdout
external frexp : float -> float * int = "caml_frexp_float"
