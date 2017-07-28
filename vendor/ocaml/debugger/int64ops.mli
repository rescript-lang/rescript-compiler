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

val ( ++ ) : int64 -> int64 -> int64;;
val ( -- ) : int64 -> int64 -> int64;;
val suc64 : int64 -> int64;;
val pre64 : int64 -> int64;;
val _0 : int64;;
val _1 : int64;;
val _minus1 : int64;;
val ( ~~ ) : string -> int64;;
val max_small_int : int64;;
val to_int : int64 -> int;;
