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

open Format;;

(* Display information about the current event. *)
val show_current_event : formatter -> unit;;

(* Display information about the current frame. *)
(* --- `select frame' must have succeeded before calling this function. *)
val show_current_frame : formatter -> bool -> unit;;

(* Display short information about one frame. *)
val show_one_frame : int -> formatter -> Instruct.debug_event -> unit
