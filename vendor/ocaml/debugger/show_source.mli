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

(* Print the line containing the point *)
val show_point : Instruct.debug_event -> bool -> unit;;

(* Tell Emacs we are nowhere in the source. *)
val show_no_point : unit -> unit;;

(* Display part of the source. *)
val show_listing :
  Lexing.position -> string -> int -> int -> int -> bool -> unit
;;
