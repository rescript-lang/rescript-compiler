(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Damien Doligez, projet Moscova, INRIA Rocqencourt          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(****************** arithmetic operators for Int64 *********************)

let ( ++ ) = Int64.add;;
let ( -- ) = Int64.sub;;
let suc64 = Int64.succ;;
let pre64 = Int64.pred;;
let _0 = Int64.zero;;
let _1 = Int64.one;;
let _minus1 = Int64.minus_one;;
let ( ~~ ) = Int64.of_string;;
let max_small_int = Int64.of_int max_int;;
let to_int = Int64.to_int;;
