(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(**************************** Time travel ******************************)

open Primitives

exception Current_checkpoint_lost
exception Current_checkpoint_lost_start_at of int64 * int64

val new_checkpoint : int -> io_channel -> unit
val set_file_descriptor : int -> io_channel -> bool
val kill_all_checkpoints : unit -> unit
val forget_process : io_channel -> int -> unit
val recover : unit -> unit

val go_to : int64 -> unit

val run : unit -> unit
val back_run : unit -> unit
val step : int64 -> unit
val finish : unit -> unit
val next : int -> unit
val start : unit -> unit
val previous : int -> unit
