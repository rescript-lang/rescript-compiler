exception Errors of exn list

let enabled = ref false

let errors = ref []

let raise err = if !enabled then errors := err :: !errors else raise err

let reportErrors () =
  if !enabled && List.length !errors > 0 then raise (Errors !errors)