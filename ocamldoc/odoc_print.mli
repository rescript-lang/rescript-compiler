(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Printing functions. *)

(** This function takes a Types.type_expr and returns a string.
   It writes in and flushes [Format.str_formatter].*)
val string_of_type_expr : Types.type_expr -> string

(** This function returns a string representing a [Types.module_type].
   @param complete indicates if we must print complete signatures
   or just [sig end]. Default if [false].
   @param code if [complete = false] and the type contains something else
   than identificators and functors, then the given code is used.
*)
val string_of_module_type : ?code: string -> ?complete: bool -> Types.module_type -> string

(** This function returns a string representing a [Types.class_type].
   @param complete indicates if we must print complete signatures
   or just [object end]. Default if [false].
*)
val string_of_class_type : ?complete: bool -> Types.class_type -> string
