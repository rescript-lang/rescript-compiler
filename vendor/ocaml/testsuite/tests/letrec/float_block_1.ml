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

(* a bug in cmmgen.ml provokes a change in compilation order between
   ocamlc and ocamlopt in certain letrec-bindings involving float
   arrays *)
let test =
  let rec x = print_endline "x"; [| 1; 2; 3 |]
      and y = print_endline "y"; [| 1.; 2.; 3. |]
  in
  assert (x = [| 1; 2; 3 |]);
  assert (y = [| 1.; 2.; 3. |]);
  ()
