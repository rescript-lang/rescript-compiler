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

(* a test with lists, because cyclic lists are fun *)
let test =
  let rec li = 0::1::2::3::4::5::6::7::8::9::li in
  match li with
    | 0::1::2::3::4::5::6::7::8::9::
        0::1::2::3::4::5::6::7::8::9::li' ->
      assert (li == li')
    | _ -> assert false
