
(* A test for stack backtraces *)

exception Error of string

let rec f msg n =
  if n = 0 then raise(Error msg) else 1 + f msg (n-1)

let g msg =
  match
    f msg 5
  with
  | _ ->
     (* value return does not happen *)
     assert false
  | exception (Error "a") ->
      print_string "a"; print_newline(); 0
  | exception (Error "b" as exn) ->
      (* this should Re-raise, appending to the current backtrace *)
      print_string "b"; print_newline(); raise exn
  | exception (Error "c") ->
      (* according to the current re-raise policy (a static condition),
         this does not re-raise *)
      raise (Error "c")

let run args =
  try
    ignore (g args.(0)); print_string "No exception\n"
  with exn ->
    Printf.printf "Uncaught exception %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout

let _ =
  Printexc.record_backtrace true;
  run [| "a" |];
  run [| "b" |];
  run [| "c" |];
  run [| "d" |];
  run [| |]
