(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Damien Doligez and Francois Rouaix, INRIA Rocquencourt           *)
(*    Ported to OCaml by John Malecki and Xavier Leroy                    *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Run-time library for profiled programs *)

val counters: (string * (string * int array)) list ref;;
val incr: int array -> int -> unit;;
