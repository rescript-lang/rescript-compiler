(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Gabriel Scherer, projet Gallium, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* a polymorphic variant of test3.ml; found a real bug once *)
let test =
  let rec x = `A f
  and f = function
    | 0 -> 2
    | n -> match x with `A g -> g 0
  in
  assert (f 1 = 2)
