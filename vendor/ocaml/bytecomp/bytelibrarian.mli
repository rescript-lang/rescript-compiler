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

(* Build libraries of .cmo files *)

(* Format of a library file:
      magic number (Config.cma_magic_number)
      absolute offset of content table
      blocks of relocatable bytecode
      content table = list of compilation units
*)

val create_archive: Format.formatter -> string list -> string -> unit

type error =
    File_not_found of string
  | Not_an_object_file of string

exception Error of error

open Format

val report_error: formatter -> error -> unit

val reset: unit -> unit
