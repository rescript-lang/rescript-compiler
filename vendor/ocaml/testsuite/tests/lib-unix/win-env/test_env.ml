external set_environment_variable: string -> string -> unit = "caml_SetEnvironmentVariable"

let find_env s =
  let env = Unix.environment () in
  let rec loop i =
    if i >= Array.length env then
      None
    else begin
      let e = env.(i) in
      let pos = String.index e '=' in
      if String.sub e 0 pos = s then
        Some (String.sub e (pos+1) (String.length e - pos - 1))
      else
        loop (i+1)
    end
  in
  loop 0

let print title = function
  | None ->
      Printf.printf "%s -> None\n%!" title
  | Some s ->
      Printf.printf "%s -> Some %S\n%!" title s

let foo = "FOO"

let () =
  set_environment_variable foo "BAR";
  print "Sys.getenv FOO" (Sys.getenv_opt foo);
  print "Unix.environment FOO" (find_env foo)
