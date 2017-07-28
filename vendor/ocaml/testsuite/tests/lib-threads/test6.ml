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

let rec f tag msg =
  select [
    send ch msg;
    wrap (receive ch) (fun x -> print_string(tag ^ ": " ^ x); print_newline())
  ];
  f tag msg

let _ =
  Thread.create (f "A") "hello";
  f "B" "world";
  exit 0
