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

(*** Program loading and initializations. ***)

val loaded : bool ref
val ensure_loaded : unit -> unit

(*** Kill program. ***)
val kill_program : unit -> unit

(* Ask whether to kill the program or not. *)
(* If yes, kill it. *)
(* Return true iff the program has been killed. *)
val ask_kill_program : unit -> bool
