(* Copyright (C) 2017 Authors of BuckleScript
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

(** {!Belt.Result}
    
    Utilities for result data type.
*)

type ('a, 'b) t =
  | Ok of 'a
  | Error of 'b

val getExn : ('a, 'b) t -> 'a
val mapWithDefaultU : ('a, 'c) t -> 'b -> ('a -> 'b [@bs]) -> 'b
val mapWithDefault : ('a, 'c) t -> 'b -> ('a -> 'b) -> 'b
val mapU : ('a, 'c) t -> ('a -> 'b [@bs]) -> ('b, 'c) t
val map : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
val flatMapU : ('a, 'c) t -> ('a -> ('b, 'c) t [@bs]) -> ('b, 'c) t
val flatMap : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
val getWithDefault : ('a, 'b) t -> 'a -> 'a
val isOk : ('a, 'b) t -> bool
val isError : ('a, 'b) t -> bool
val eqU : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> bool [@bs]) -> bool
val eq : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> bool) -> bool
val cmpU : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> int [@bs]) -> int
val cmp : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> int) -> int