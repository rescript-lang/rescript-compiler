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

let private_data = (Hashtbl.create 17 : (Thread.t, string) Hashtbl.t)
let private_data_lock = Mutex.create()
let output_lock = Mutex.create()

let set_private_data data =
  Mutex.lock private_data_lock;
  Hashtbl.add private_data (Thread.self()) data;
  Mutex.unlock private_data_lock

let get_private_data () =
  Hashtbl.find private_data (Thread.self())

let process id data =
  set_private_data data;
  Mutex.lock output_lock;
  print_int id; print_string " --> "; print_string(get_private_data());
  print_newline();
  Mutex.unlock output_lock

let _ =
  let t1 = Thread.create (process 1) "un" in
  let t2 = Thread.create (process 2) "deux" in
  let t3 = Thread.create (process 3) "trois" in
  let t4 = Thread.create (process 4) "quatre" in
  let t5 = Thread.create (process 5) "cinq" in
  List.iter Thread.join [t1;t2;t3;t4;t5]
