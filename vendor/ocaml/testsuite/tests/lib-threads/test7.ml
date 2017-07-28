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

let add_ch = new_channel()
let sub_ch = new_channel()
let read_ch = new_channel()

let rec accu n =
  select [
    wrap (receive add_ch) (fun x -> accu (n+x));
    wrap (receive sub_ch) (fun x -> accu (n-x));
    wrap (send read_ch n) (fun () -> accu n)
  ]

let rec sender chan value =
  sync(send chan value); sender chan value

let read () =
  print_int(sync(receive read_ch)); print_newline()

let main () =
  Thread.create accu 0;
  Thread.create (sender add_ch) 1;
  Thread.create (sender sub_ch) 1;
  while true do read() done

let _ = Printexc.catch main ()
