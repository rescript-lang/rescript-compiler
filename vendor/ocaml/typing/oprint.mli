(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Projet Cristal, INRIA Rocquencourt                 *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format
open Outcometree

#if undefined BS_NO_COMPILER_PATCH then
val out_ident : (formatter -> string -> unit) ref
#end
val out_value : (formatter -> out_value -> unit) ref
val out_type : (formatter -> out_type -> unit) ref
val out_class_type : (formatter -> out_class_type -> unit) ref
val out_module_type : (formatter -> out_module_type -> unit) ref
val out_sig_item : (formatter -> out_sig_item -> unit) ref
val out_signature : (formatter -> out_sig_item list -> unit) ref
val out_type_extension : (formatter -> out_type_extension -> unit) ref
val out_phrase : (formatter -> out_phrase -> unit) ref

val parenthesized_ident : string -> bool
