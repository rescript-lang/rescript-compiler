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
(* Adapted for Javascript backend: Hongbo Zhang,                       *)


(** A pass used to optimize the exit code compilation, adaped from the compiler's
    [simplif] module
 *)

val count_helper : Lam.t -> (int, int ref) Hashtbl.t

type subst_tbl = (int, Ident.t list * Lam.t) Hashtbl.t

val subst_helper : subst_tbl -> (int -> int) -> Lam.t -> Lam.t

val simplify_exits : Lam.t -> Lam.t
