
(* Copyright (C) 2017 Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** A small module to provide a inclusive range operations from `start` to `finish`.
    These use a for-loop internally instead of creating an array
*)

val forEachU: int -> int -> (int -> unit [@bs]) -> unit
val forEach: int -> int -> (int -> unit ) -> unit
(** `forEach start finish action`

    equivalent to `Belt.Array.(forEach (range start finish) action)`
*)

val everyU: int -> int -> (int -> bool [@bs]) -> bool
val every: int -> int -> (int -> bool ) -> bool
(** `every start finish p`

    equivalent to `Belt.Array.(every (range start finish) p )`
*)

val everyByU: int -> int -> step:int -> (int -> bool [@bs]) -> bool
val everyBy: int -> int -> step:int -> (int -> bool ) -> bool
(** `everyBy start finish ~step p`

    **See** [`Belt_Array.rangeBy`]()

    equivalent to `Belt.Array.(every (rangeBy start finish ~step) p)`
*)

val someU: int -> int -> (int -> bool [@bs]) -> bool
val some: int -> int -> (int -> bool ) -> bool
(** `some start finish p`

    equivalent to `Belt.Array.(some (range start finish) p)`
*)

val someByU: int -> int -> step:int -> (int -> bool [@bs]) -> bool
val someBy: int -> int -> step:int -> (int -> bool ) -> bool
(** `someBy start finish ~step  p`

    **See** [`Belt_Array.rangeBy`]()

    equivalent to `Belt.Array.(some (rangeBy start finish ~step) p)`
*)
