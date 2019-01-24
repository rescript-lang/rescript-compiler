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

(* From lambda to assembly code *)

val compile_implementation_flambda :
    ?toplevel:(string -> bool) ->
    string ->
    required_globals:Ident.Set.t ->
    backend:(module Backend_intf.S) ->
    Format.formatter -> Flambda.program -> unit

val compile_implementation_clambda :
    ?toplevel:(string -> bool) ->
    string ->
    Format.formatter -> Lambda.program -> unit

val compile_phrase :
    Format.formatter -> Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: Format.formatter -> error -> unit


val compile_unit:
  string(*prefixname*) ->
  string(*asm file*) -> bool(*keep asm*) ->
  string(*obj file*) -> (unit -> unit) -> unit
