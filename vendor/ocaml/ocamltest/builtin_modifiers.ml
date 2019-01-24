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

(* Definition of a few built-in environment modifiers *)

open Environments
open Builtin_variables

let expect =
[
  Replace (script, "bash ${OCAMLSRCDIR}/testsuite/tools/expect");
]

let principal =
[
  Append (flags, " -principal ");
  Add (compiler_directory_suffix, ".principal");
  Add (compiler_reference_suffix, ".principal");
]

let testinglib_directory = Ocamltest_config.ocamlsrcdir ^ "/testsuite/lib"

let testing =
[
  Append (flags, (" -I " ^ testinglib_directory ^ " "));
  Append (libraries, " testing ");
]

let _ =
  register expect "expect";
  register principal "principal";
  register testing "testing"
