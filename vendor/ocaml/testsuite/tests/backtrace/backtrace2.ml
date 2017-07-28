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
