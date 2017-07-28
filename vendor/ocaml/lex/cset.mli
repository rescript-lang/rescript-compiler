(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Luc Maranget, Jerome Vouillon projet Cristal,            *)
(*                         INRIA Rocquencourt                          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Set of characters encoded as list of intervals *)

type t

val empty : t
val is_empty : t -> bool
val all_chars : t
exception Bad

val all_chars_eof : t
val eof : t
val singleton : int ->  t
val interval : int -> int -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val complement : t -> t
val env_to_array : (t * 'a) list -> 'a array
