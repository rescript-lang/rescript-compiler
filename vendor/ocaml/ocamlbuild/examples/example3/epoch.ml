(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let _ =
  let s = Num.num_of_string (Printf.sprintf "%.0f" (Unix.gettimeofday ())) in
  let ps = Num.mult_num (Num.num_of_string "1000000000000") s in
  Printf.printf "%s picoseconds have passed since January 1st, 1970.\n"
    (Num.string_of_num ps)
;;
