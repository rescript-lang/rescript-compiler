(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Parsetree;;
open Format;;

val interface : formatter -> signature_item list -> unit;;
val implementation : formatter -> structure_item list -> unit;;
val top_phrase : formatter -> toplevel_phrase -> unit;;

val expression: int -> formatter -> expression -> unit
val structure: int -> formatter -> structure -> unit
val payload: int -> formatter -> payload -> unit
