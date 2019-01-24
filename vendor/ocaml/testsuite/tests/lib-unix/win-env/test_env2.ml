(* This test is disabled (see test_env2.precheck) as it fails due to MPR#4499:
   the Windows POSIX environment does not get updated when using the native
   Windows API SetEnvironmentVariable. *)

external set_environment_variable: string -> string -> unit = "caml_SetEnvironmentVariable"

let print title = function
  | None ->
      Printf.printf "%s -> None\n%!" title
  | Some s ->
      Printf.printf "%s -> Some %S\n%!" title s

let foo = "FOO"

let () =
  set_environment_variable foo "BAR";
  print "Sys.getenv FOO" (Sys.getenv_opt foo)
