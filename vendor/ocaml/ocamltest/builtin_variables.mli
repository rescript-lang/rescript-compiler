(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of variables used by built-in actions *)

(* The variables are listed in alphabetical order *)

val arguments : Variables.t

val c_preprocessor : Variables.t

val compiler_directory_suffix : Variables.t

val compiler_reference : Variables.t

val compiler_reference2 : Variables.t

val compiler_reference_suffix : Variables.t

val compiler_output : Variables.t

val compiler_output2 : Variables.t

val files : Variables.t

val flags : Variables.t

val libraries : Variables.t

val modules : Variables.t

val ocamlc_flags : Variables.t
val ocamlc_default_flags : Variables.t

val ocamlopt_flags : Variables.t
val ocamlopt_default_flags : Variables.t

val ocaml_byte_exit_status : Variables.t

val ocamlc_byte_exit_status : Variables.t

val ocamlopt_byte_exit_status : Variables.t

val ocaml_opt_exit_status : Variables.t

val ocamlc_opt_exit_status : Variables.t

val ocamlopt_opt_exit_status : Variables.t

val output : Variables.t

val program : Variables.t
val program2 : Variables.t

val reference : Variables.t

val script : Variables.t

val stdin : Variables.t
val stdout : Variables.t
val stderr : Variables.t

val test_build_directory : Variables.t

val test_file : Variables.t

val test_source_directory : Variables.t
