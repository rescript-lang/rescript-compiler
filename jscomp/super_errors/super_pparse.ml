

(* taken from https://github.com/rescript-lang/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/driver/pparse.ml#L170 *)
(* modified branches are commented *)
let report_error ppf () = 
  Format.fprintf ppf
    "@[<v>@{<info>There's been an error running Reason's parser on a file.@}@,\
     If the message doesn't help, check for errors slightly above.@,\
     @[Please file an issue on@ github.com/facebook/reason.@ Thanks!@]@]"


let setup () =
  Location.register_error_of_exn
    (function
      | Ast_reason_pp.Pp_error -> 
        Some (Super_location.error_of_printer_file report_error ())
      | _ -> None
    )
