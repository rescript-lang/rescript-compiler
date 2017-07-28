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

open Event

let ch = (new_channel() : string channel)

let rec sender msg =
  sync (send ch msg);
  sender msg

let rec receiver name =
  print_string (name ^ ": " ^ sync (receive ch) ^ "\n");
  flush stdout;
  receiver name

let _ =
  Thread.create sender "hello";
  Thread.create sender "world";
  Thread.create receiver "A";
  receiver "B";
  exit 0
