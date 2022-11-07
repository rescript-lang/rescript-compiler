(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compiling C files and building C libraries *)

val command: string -> int
val run_command: string -> unit
val compile_file: ?output:string -> ?opt:string -> string -> int
val create_archive: string -> string list -> int
val expand_libname: string -> string
val quote_files: string list -> string
val quote_optfile: string option -> string
(*val make_link_options: string list -> string*)

type link_mode =
  | Exe
  | Dll
  | MainDll
  | Partial

val call_linker: link_mode -> string -> string list -> string -> bool
