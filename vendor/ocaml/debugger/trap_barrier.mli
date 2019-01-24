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

(************************* Trap barrier ********************************)

val install_trap_barrier : int -> unit

val remove_trap_barrier : unit -> unit

(* Ensure the trap barrier state is up to date in current checkpoint. *)
val update_trap_barrier : unit -> unit

(* Execute `funct' with a trap barrier. *)
(* --- Used by `finish'. *)
val exec_with_trap_barrier : int -> (unit -> unit) -> unit
