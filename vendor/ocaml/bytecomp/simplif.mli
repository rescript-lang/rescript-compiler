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

(* Elimination of useless Llet(Alias) bindings.
   Transformation of let-bound references into variables.
   Simplification over staticraise/staticcatch constructs.
   Generation of tail-call annotations if -annot is set. *)

open Lambda

val simplify_exits: lambda -> lambda
val eliminate_ref: Lambda.IdentSet.elt -> Lambda.lambda -> Lambda.lambda
val simplify_lets: Lambda.lambda -> Lambda.lambda
val simplify_lambda: lambda -> lambda

(* To be filled by asmcomp/selectgen.ml *)
val is_tail_native_heuristic: (int -> bool) ref
                          (* # arguments -> can tailcall *)
