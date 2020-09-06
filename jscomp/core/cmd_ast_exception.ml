type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error


let report_error ppf = function
  | CannotRun cmd ->
      Format.fprintf ppf "Error while running external preprocessor@.\
                   Command line: %s@." cmd
  | WrongMagic cmd ->
      Format.fprintf ppf "External preprocessor does not produce a valid file@.\
                   Command line: %s@." cmd

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let cannot_run comm =
  raise (Error (CannotRun comm))

let wrong_magic magic =
    raise (Error (WrongMagic magic))