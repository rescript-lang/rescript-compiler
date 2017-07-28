(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Original author: Nicolas Pouillard *)

val is_simple_filename : string -> bool

val quote_filename_if_needed : string -> string
(** This will quote using Unix conventions, even on Windows, because commands are
 * always run through bash -c on Windows. *)

val chdir : string -> unit
val rm : string -> unit
val rm_f : string -> unit
val rm_rf : string -> unit
val mkdir : string -> unit
val try_mkdir : string -> unit
val mkdir_p : string -> unit
val cp : string -> string -> unit
val mv : string -> string -> unit
val readlink : string -> string
val is_link : string -> bool
