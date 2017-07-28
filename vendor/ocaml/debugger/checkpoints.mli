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

(***************************** Checkpoints *****************************)

open Primitives
open Debugcom

(*** A type for checkpoints. ***)

type checkpoint_state =
    C_stopped
  | C_running of int64

(* `c_valid' is true if and only if the corresponding
 * process is connected to the debugger.
 * `c_parent' is the checkpoint whose process is parent
 * of the checkpoint one (`root' if no parent).
 * c_pid =  2 for root pseudo-checkpoint.
 * c_pid =  0 for ghost checkpoints.
 * c_pid = -1 for kill checkpoints.
 *)
type checkpoint =
  {mutable c_time : int64;
   mutable c_pid : int;
   mutable c_fd : io_channel;
   mutable c_valid : bool;
   mutable c_report : report option;
   mutable c_state : checkpoint_state;
   mutable c_parent : checkpoint;
   mutable c_breakpoint_version : int;
   mutable c_breakpoints : (int * int ref) list;
   mutable c_trap_barrier : int}

(*** Pseudo-checkpoint `root'. ***)
(* --- Parents of all checkpoints which have no parent. *)
val root : checkpoint

(*** Current state ***)
val checkpoints : checkpoint list ref
val current_checkpoint : checkpoint ref

val current_time : unit -> int64
val current_report : unit -> report option
val current_pc : unit -> int option
val current_pc_sp : unit -> (int * int) option
