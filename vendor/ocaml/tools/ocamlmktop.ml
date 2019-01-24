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

let _ =
  let args = Ccomp.quote_files (List.tl (Array.to_list Sys.argv)) in
  let ocamlmktop = Sys.executable_name in
  (* On Windows Sys.command calls system() which in turn calls 'cmd.exe /c'.
     cmd.exe has special quoting rules (see 'cmd.exe /?' for details).
     Short version: if the string passed to cmd.exe starts with '"',
     the first and last '"' are removed *)
  let ocamlc,extra_quote =
    if Sys.win32 then "ocamlc.exe","\"" else "ocamlc",""
  in
  let ocamlc = Filename.(quote (concat (dirname ocamlmktop) ocamlc)) in
  let cmdline =
    extra_quote ^ ocamlc ^ " -I +compiler-libs -linkall ocamlcommon.cma " ^
    "ocamlbytecomp.cma ocamltoplevel.cma " ^ args ^ " topstart.cmo" ^
    extra_quote
  in
  exit(Sys.command cmdline)
