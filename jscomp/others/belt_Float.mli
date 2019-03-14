(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(* Adapted significantly by Authors of BuckleScript *)


(** {!Belt.Float}
    Utililites for Float
*)

external toInt: float -> int = "%intoffloat"

external fromInt: int -> float = "%identity"

external fromString: string -> float = "parseFloat" [@@bs.val]

external toString: float -> string = "String" [@@bs.val]

val (+): float -> float -> float

val (-): float -> float -> float

val (/): float -> float -> float

val ( * ): float -> float -> float
