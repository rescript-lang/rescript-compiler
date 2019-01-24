(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pretty-printing of C-- code *)

open Format

val rec_flag : formatter -> Cmm.rec_flag -> unit
val machtype_component : formatter -> Cmm.machtype_component -> unit
val machtype : formatter -> Cmm.machtype_component array -> unit
val comparison : Cmm.comparison -> string
val chunk : Cmm.memory_chunk -> string
val operation : Debuginfo.t -> Cmm.operation -> string
val expression : formatter -> Cmm.expression -> unit
val fundecl : formatter -> Cmm.fundecl -> unit
val data : formatter -> Cmm.data_item list -> unit
val phrase : formatter -> Cmm.phrase -> unit
val raise_kind: formatter -> Cmm.raise_kind -> unit
