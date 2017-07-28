(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Luc Maranget projet Moscova INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

val output_lexdef :
  string ->
  in_channel ->
  out_channel ->
  Common.line_tracker ->
  Syntax.location ->
  Syntax.location option ->
  (string list, Syntax.location) Lexgen.automata_entry list ->
  Lexgen.automata array -> Syntax.location -> unit
