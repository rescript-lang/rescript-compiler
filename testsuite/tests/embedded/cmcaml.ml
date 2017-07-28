(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* OCaml part of the code *)

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let format_result n =
  let r = "Result = " ^ string_of_int n in
  (* Allocate gratuitously to test GC *)
  for i = 1 to 1500 do ignore (Bytes.create 256) done;
  r

(* Registration *)

let _ =
  Callback.register "fib" fib;
  Callback.register "format_result" format_result
