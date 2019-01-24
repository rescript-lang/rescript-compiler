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

(* Types of input files involved in an OCaml project and related functions *)

type t =
  | Implementation
  | Interface
  | C
  | C_minus_minus
  | Lexer
  | Grammar

val string_of_filetype : t -> string

val extension_of_filetype : t -> string

val filetype_of_extension : string -> t

val split_filename : string -> string * string

val filetype : string -> string * t

val make_filename : string * t -> string
