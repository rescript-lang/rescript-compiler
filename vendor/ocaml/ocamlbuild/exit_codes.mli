(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

val rc_ok                              : int
val rc_usage                           : int
val rc_failure                         : int
val rc_invalid_argument                : int
val rc_system_error                    : int
val rc_hygiene                         : int
val rc_circularity                     : int
val rc_solver_failed                   : int
val rc_ocamldep_error                  : int
val rc_lexing_error                    : int
val rc_build_error                     : int
val rc_executor_subcommand_failed      : int
val rc_executor_subcommand_got_signal  : int
val rc_executor_io_error               : int
val rc_executor_excetptional_condition : int
