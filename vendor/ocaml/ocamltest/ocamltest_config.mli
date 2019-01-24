(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interface for ocamltest's configuration module *)

val arch : string
(** Architecture for the native compiler, "none" if it is disabled *)

val c_preprocessor : string
(** Command to use to invoke the C preprocessor *)


val ocamlc_default_flags : string
(** Flags passed by default to ocamlc.byte and ocamlc.opt *)

val ocamlopt_default_flags : string
(** Flags passed by default to ocamlopt.byte and ocamlopt.opt *)

val ocamlsrcdir : string
(** The absolute path of the directory containing the sources of OCaml *)

val flambda : bool
(** Whether flambda has been enabled at configure time *)

val safe_string : bool
(** Whether the compiler was configured with -safe-string *)
