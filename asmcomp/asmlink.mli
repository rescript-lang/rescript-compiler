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

(* Link a set of .cmx/.o files and produce an executable or a plugin *)

open Format

val link: formatter -> string list -> string -> unit

val link_shared: formatter -> string list -> string -> unit

val call_linker_shared: string list -> string -> unit

val reset : unit -> unit
val check_consistency: string -> Cmx_format.unit_infos -> Digest.t -> unit
val extract_crc_interfaces: unit -> (string * Digest.t option) list
val extract_crc_implementations: unit -> (string * Digest.t option) list

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of (string * string list) list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Assembler_error of string
  | Linking_error
  | Multiple_definition of string * string * string
  | Missing_cmx of string * string

exception Error of error

val report_error: formatter -> error -> unit
