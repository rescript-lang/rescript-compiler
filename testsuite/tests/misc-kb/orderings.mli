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

open Terms

type ordering =
    Greater
  | Equal
  | NotGE

val ge_ord: ('a -> ordering) -> 'a -> bool
val gt_ord: ('a -> ordering) -> 'a -> bool
val eq_ord: ('a -> ordering) -> 'a -> bool
val rem_eq: ('a * 'b -> bool) -> 'a -> 'b list -> 'b list
val diff_eq: ('a * 'a -> bool) -> 'a list * 'a list -> 'a list * 'a list
val mult_ext: (term * term -> ordering) -> term * term -> ordering
val lex_ext: (term * term -> ordering) -> term * term -> ordering
val rpo: (string -> string -> ordering) ->
         ((term * term -> ordering) -> term * term -> ordering) ->
         term * term -> ordering
