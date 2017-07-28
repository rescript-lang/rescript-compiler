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

let yield = ref false

let print_message c =
  for i = 1 to 10000 do
    print_char c; flush stdout;
    if !yield then Thread.yield()
  done

let _ = yield := (Array.length Sys.argv > 1)
let t1 = Thread.create print_message 'a'
let t2 = Thread.create print_message 'b'
let _ = Thread.join t1
let _ = Thread.join t2

;;
