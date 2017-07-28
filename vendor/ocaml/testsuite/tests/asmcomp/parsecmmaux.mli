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

(* Auxiliary functions for parsing *)

val bind_ident: string -> Ident.t
val find_ident: string -> Ident.t
val unbind_ident: Ident.t -> unit

type error =
    Unbound of string

exception Error of error

val report_error: error -> unit
