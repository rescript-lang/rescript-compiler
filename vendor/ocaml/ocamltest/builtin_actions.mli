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

(* Definition of a few built-in actions *)

val compile_bytecode_with_bytecode_compiler : Actions.t
val compile_bytecode_with_native_compiler : Actions.t
val compile_native_with_bytecode_compiler : Actions.t
val compile_native_with_native_compiler : Actions.t

val execute : Actions.t
val expect : Actions.t
val script : Actions.t
val check_program_output : Actions.t

val compare_bytecode_programs : Actions.t
val compare_native_programs : Actions.t

val check_ocamlc_dot_byte_output : Actions.t
val check_ocamlc_dot_opt_output : Actions.t
val check_ocamlopt_dot_byte_output : Actions.t
val check_ocamlopt_dot_opt_output : Actions.t

val run_in_ocaml : Actions.t

val run_in_ocamlnat : Actions.t

val check_ocaml_output : Actions.t

val check_ocamlnat_output : Actions.t
val if_not_safe_string : Actions.t
