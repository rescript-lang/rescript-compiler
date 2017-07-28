(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* A test for stack backtraces *)

exception Error of string

let rec f msg n =
  if n = 0 then raise(Error msg) else 1 + f msg (n-1)

let g msg =
  try
    f msg 5
  with Error "a" -> print_string "a"; print_newline(); 0
     | Error "b" as exn -> print_string "b"; print_newline(); raise exn
     | Error "c" -> raise (Error "c")

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
      Printexc.print_raw_backtrace stdout trace

let _ =
  Printexc.record_backtrace true;
  run [| "a" |];
  run [| "b" |];
  run [| "c" |];
  run [| "d" |];
  run [| |]
