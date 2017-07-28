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

let output_lock = Mutex.create()

let rec fib n = if n <= 2 then 1 else fib(n-1) + fib(n-2)

let fibtask n =
  while true do
    let res = fib n in
    Mutex.lock output_lock;
    print_int res; print_newline();
    Mutex.unlock output_lock
  done

let _ =
  Thread.create fibtask 28;
  Thread.delay 1.0;
  while true do
    let l = read_line () in
    Mutex.lock output_lock;
    print_string ">> "; print_string l; print_newline();
    Mutex.unlock output_lock
  done
