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


(** {!Belt.Int}
    Utililites for Int
*)

external toFloat: int -> float = "%identity"

external fromFloat: float -> int = "%intoffloat"

external fromString: string -> (_ [@bs.as 10]) -> int = "parseInt" [@@bs.val]

external toString: int -> string = "String" [@@bs.val]

let (+) = (+)

let (-) = (-)

let (/) = (/)

let ( * ) = ( * )
