open Printf

let _ =
  let arg = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let env = Array.append [|"FOO=foo"|] (Unix.environment()) in
  try
    Unix.execvpe arg.(0) arg env
  with
  | Unix.Unix_error(Unix.ENOENT, _, arg) ->
      eprintf "No such file %s\n" arg; exit 2
  | Unix.Unix_error(Unix.EACCES, _, arg) ->
      eprintf "Permission denied %s\n" arg; exit 2
  | Unix.Unix_error(err, fn, arg) ->
      eprintf "Other error %s - %s - %s\n" (Unix.error_message err) fn arg;
      exit 4
