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


(* Original author: Berke Durak *)
(* Ocamlbuild_executor *)

(** UNIX-specific module for running tasks in parallel and properly multiplexing their outputs. *)

type error =
  | Subcommand_failed
  | Subcommand_got_signal
  | Io_error
  | Exceptionl_condition

(** [execute ~ticker ~period ~display ~exit commands] will execute the commands
    in [commands] in parallel, correctly multiplexing their outputs.

    A  command  is  a function that given a unit [()] returns the shell command
    string  to  execute,  commands  are  functions in order to do some job just
    before  executing  the  command.  These  functions  will be called once. If
    specified,  it  will  call  [ticker]  at  least  every [period] seconds. If
    specified,  it  will  call  [display  f] when it wishes to print something;
    [display]  should  then  call  [f]  with  then  channel on which [f] should
    print.

    Note  that  if  the shell command to execute is the empty string [""], it's
    considered as a no-op.

    Note  that  [f] must be idempotent as it may well be called twice, once for
    the log file, once for the actual output.

    If  one of the commands fails, it will exit with an appropriate error code,
    calling [cleanup] before.

    All  exits  are  done  trough the call to the given [exit] function, if not
    supplied Pervasives.exit is used.
*)
val execute :
  ?max_jobs:int ->
  ?ticker:(unit -> unit) ->
  ?period:float ->
  ?display:((out_channel -> unit) -> unit) ->
   exit:(error -> unit) ->
    ((unit -> string) list list) ->
    (bool list * exn) option
