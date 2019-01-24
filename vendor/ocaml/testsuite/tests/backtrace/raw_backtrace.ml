
(* A test for stack backtraces *)

exception Error of string

let rec f msg n =
  if n = 0 then raise(Error msg) else 1 + f msg (n-1)

exception Localized of exn

let g msg =
  let exception_raised_internally () =
    try Hashtbl.find (Hashtbl.create 3) 0
    with Not_found -> false in
  try
    f msg 5
  with Error "a" -> print_string "a"; print_newline(); 0
     | Error "b" as exn -> print_string "b"; print_newline(); raise exn
     | Error "c" -> raise (Error "c")
     (** [Error "d"] not caught *)
     | Error "e" as exn ->
         let bt = Printexc.get_raw_backtrace () in
         print_string "e"; print_newline ();
         ignore (exception_raised_internally ());
         Printexc.raise_with_backtrace exn bt
     | Error "f" as exn ->
         let bt = Printexc.get_raw_backtrace () in
         print_string "f"; print_newline ();
         Printexc.raise_with_backtrace (Localized exn) bt

let backtrace args =
  try
    ignore (g args.(0)); None
  with exn ->
    let exn = Printexc.to_string exn in
    let trace = Printexc.get_raw_backtrace () in
    Some (exn, trace)

let run args =
  match backtrace args with
    | None -> print_string "No exception\n"
    | Some (exn, trace) ->
      begin
        (* raise another exception to stash the global backtrace *)
        try ignore (f "c" 5); assert false with Error _ -> ();
      end;
      Printf.printf "Uncaught exception %s\n" exn;
      Printexc.print_raw_backtrace stdout trace;
      flush stdout

let _ =
  Printexc.record_backtrace true;
  run [| "a" |];
  run [| "b" |];
  run [| "c" |];
  run [| "d" |];
  run [| "e" |];
  run [| "f" |];
  run [| |]
