(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Typedtree
open Lambda

val transl_class :
  Ident.t list -> Ident.t ->
  string list -> class_expr -> Asttypes.virtual_flag -> lambda;;

type error = Illegal_class_expr | Tags of string * string

exception Error of Location.t * error

open Format

val report_error: formatter -> error -> unit
