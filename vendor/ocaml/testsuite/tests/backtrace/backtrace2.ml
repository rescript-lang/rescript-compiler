
(* A test for stack backtraces *)

exception Error of string

let test_Error msg =
  let rec f msg n =
    if n = 0 then raise(Error msg) else 1 + f msg (n-1) in
  let exception_raised_internally () =
    try Hashtbl.find (Hashtbl.create 3) 0
    with Not_found -> false in
  try
    f msg 5
  with Error "a" -> print_string "a"; print_newline(); 0
     | Error "b" as exn -> print_string "b"; print_newline(); raise exn
     | Error "c" -> raise (Error "c")
     (** [Error "d"] not caught *)
     (** Test reraise when an exception is used in the middle of the exception
         handler. Currently the wrong backtrace is used. *)
     | Error "e" as exn ->
         print_string "e"; print_newline ();
         ignore (exception_raised_internally ()); raise exn
     (** Test reraise of backtrace when a `when` clause use exceptions.
         Currently the wrong backtrace is used.
     *)
     | Error "f" when exception_raised_internally () ->
         assert false (** absurd: when false *)
     | Error "f" as exn -> print_string "f"; print_newline(); raise exn

let test_Not_found () =
  let rec aux n =
    if n = 0 then raise Not_found else 1 + aux (n-1)
  in
  try aux 5
  (** Test the raise to reraise heuristic with included try_with.
      Currently the wrong backtrace is used. *)
  with exn ->
    print_string "test_Not_found"; print_newline();
    (try Hashtbl.find (Hashtbl.create 3) 0 with Not_found -> raise exn)

let test_lazy =
  let rec aux n =
    if n = 0 then raise Not_found else 1 + aux (n-1)
  in
  let exception_raised_internally () =
    try Hashtbl.find (Hashtbl.create 3) 0
    with Not_found -> () in
  let l = lazy (aux 5) in
  (** Test the backtrace obtained from a lazy value.
      Currently the second time the value is forced the
      wrong backtrace is used. *)
  fun () ->
    exception_raised_internally ();
    Lazy.force l

let run g args =
  try
    ignore (g args.(0)); print_string "No exception\n"
  with exn ->
    Printf.printf "Uncaught exception %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout

let _ =
  Printexc.record_backtrace true;
  run test_Error [| "a" |];
  run test_Error [| "b" |];
  run test_Error [| "c" |];
  run test_Error [| "d" |];
  run test_Error [| "e" |];
  run test_Error [| "f" |];
  run test_Error [| |];
  run test_Not_found  [| () |];
  run test_lazy  [| () |];
  run test_lazy  [| () |];
  ()
