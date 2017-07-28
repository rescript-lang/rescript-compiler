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

(* Classic producer-consumer *)

type 'a prodcons =
  { buffer: 'a array;
    lock: Mutex.t;
    mutable readpos: int;
    mutable writepos: int;
    notempty: Condition.t;
    notfull: Condition.t }

let create size init =
  { buffer = Array.make size init;
    lock = Mutex.create();
    readpos = 0;
    writepos = 0;
    notempty = Condition.create();
    notfull = Condition.create() }

let output_lock = Mutex.create()

let put p data =
  Mutex.lock p.lock;
  while (p.writepos + 1) mod Array.length p.buffer = p.readpos do
    Condition.wait p.notfull p.lock
  done;
  p.buffer.(p.writepos) <- data;
  p.writepos <- (p.writepos + 1) mod Array.length p.buffer;
  Condition.signal p.notempty;
  Mutex.unlock p.lock

let get p =
  Mutex.lock p.lock;
  while p.writepos = p.readpos do
    Condition.wait p.notempty p.lock
  done;
  let data = p.buffer.(p.readpos) in
  p.readpos <- (p.readpos + 1) mod Array.length p.buffer;
  Condition.signal p.notfull;
  Mutex.unlock p.lock;
  data

(* Test *)

let buff = create 20 0

let rec produce n =
  Mutex.lock output_lock;
  print_int n; print_string "-->"; print_newline();
  Mutex.unlock output_lock;
  put buff n;
  if n < 10000 then produce (n+1)

let rec consume () =
  let n = get buff in
  Mutex.lock output_lock;
  print_string "-->"; print_int n; print_newline();
  Mutex.unlock output_lock;
  if n < 10000 then consume ()

let t1 = Thread.create produce 0
let _ = consume ()

;;
