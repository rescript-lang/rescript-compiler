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

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let _ =
  let n =
    if Array.length Sys.argv >= 2
    then int_of_string Sys.argv.(1)
    else 40 in
  print_int(fib n); print_newline(); exit 0
