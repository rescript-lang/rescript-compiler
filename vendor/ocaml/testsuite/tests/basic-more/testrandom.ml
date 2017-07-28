(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Random

let _ =
  for i = 0 to 20 do
    print_char ' '; print_int (int 1000);
  done;
  print_newline ();  print_newline ();
  for i = 0 to 20 do
    print_char ' '; print_float (float 1000.);
  done

let _ = exit 0
