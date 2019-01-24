(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(************************** Trap barrier *******************************)

open Debugcom
open Checkpoints

let current_trap_barrier = ref 0

let install_trap_barrier pos =
  current_trap_barrier := pos

let remove_trap_barrier () =
  current_trap_barrier := 0

(* Ensure the trap barrier state is up to date in current checkpoint. *)
let update_trap_barrier () =
  if !current_checkpoint.c_trap_barrier <> !current_trap_barrier then
    Exec.protect
      (function () ->
         set_trap_barrier !current_trap_barrier;
         !current_checkpoint.c_trap_barrier <- !current_trap_barrier)

(* Execute `funct' with a trap barrier. *)
(* --- Used by `finish'. *)
let exec_with_trap_barrier trap_barrier funct =
  try
    install_trap_barrier trap_barrier;
    funct ();
    remove_trap_barrier ()
  with
    x ->
      remove_trap_barrier ();
      raise x
