(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Luc Maranget projet Moscova INRIA Rocquencourt             *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val output_lexdef :
  in_channel ->
  out_channel ->
  Common.line_tracker ->
  Syntax.location ->
  Syntax.location option ->
  (string list, Syntax.location) Lexgen.automata_entry list ->
  Lexgen.automata array -> Syntax.location -> unit
