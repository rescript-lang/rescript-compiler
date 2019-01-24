let () =
  let fd = Unix.openfile "plop" [O_CREAT; O_WRONLY] 0o666 in
  let pid =
    Unix.create_process "echo" [|"echo"; "toto"|] Unix.stdin fd Unix.stderr
  in
  Unix.close fd;
  while fst (Unix.waitpid [WNOHANG] pid) = 0 do
    Unix.sleepf 0.001
  done;
  match Sys.remove "plop" with
  | () ->  print_endline "OK"
  | exception (Sys_error _) -> print_endline "ERROR"
