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

(* Miscellaneous library functions *)

val concatmap : ('a -> 'b list) -> 'a list -> 'b list

val is_blank : char -> bool

val maybe_quote : string -> string

val words : string -> string list

val file_is_empty : string -> bool

val string_of_location: Location.t -> string

val run_system_command : string -> unit

val make_directory : string -> unit

val string_of_file : string -> string

val copy_file : string -> string -> unit
