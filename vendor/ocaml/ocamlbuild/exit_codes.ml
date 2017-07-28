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

let rc_ok                              = 0
let rc_usage                           = 1
let rc_failure                         = 2
let rc_invalid_argument                = 3
let rc_system_error                    = 4
let rc_hygiene                         = 1
let rc_circularity                     = 5
let rc_solver_failed                   = 6
let rc_ocamldep_error                  = 7
let rc_lexing_error                    = 8
let rc_build_error                     = 9
let rc_executor_subcommand_failed      = 10
let rc_executor_subcommand_got_signal  = 11
let rc_executor_io_error               = 12
let rc_executor_excetptional_condition = 13
