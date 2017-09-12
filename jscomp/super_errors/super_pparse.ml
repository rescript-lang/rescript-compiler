let fprintf = Format.fprintf

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/driver/pparse.ml#L170 *)
(* modified branches are commented *)
let report_error ppf = function
  | Pparse.CannotRun cmd ->
    (* modified *)
    if Ext_string.contain_substring cmd "refmt" then
      fprintf ppf 
        "@[<v>@{<info>There's been an error running Reason's refmt parser on a file.@}@,\
          This was the command:@,@,%s@,@,\
          @[Please file an issue on@ github.com/facebook/reason.@ Thanks!@]@]" cmd
    else 
      fprintf ppf "@[<v>@{<info>There's been an error running a preprocessor before the compilation of a file.@}@,\
                   This was the command:@,@,%s@]" cmd
  | WrongMagic cmd ->
      fprintf ppf "External preprocessor does not produce a valid file@.\
                   Command line: %s@." cmd

let setup () =
  Location.register_error_of_exn
    (function
      | Pparse.Error err -> Some (Super_location.error_of_printer_file report_error err)
      | _ -> None
    )
