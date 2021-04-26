# 1 "others/setm.cppo.mli"
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

(**
  This module is [`Belt.MutableSet`]() specialized with key type to be a primitive type.

  It is more efficient in general, the  API is the same with [`Belt.MutableSet`]() except its key type is fixed,
  and identity is not needed(using the built-in one)

  **See** [`Belt.MutableSet`]()
*)


# 38 "others/setm.cppo.mli"
type value = int
  
# 42 "others/setm.cppo.mli"
  (** The type of the set elements. *)


type t
(** The type of sets. *)

val make: unit -> t

val fromArray: value array -> t
val fromSortedArrayUnsafe: value array -> t

val copy: t -> t
val isEmpty: t -> bool
val has: t -> value -> bool

val add: t -> value -> unit
val addCheck: t -> value -> bool
val mergeMany: t -> value array -> unit
val remove: t -> value -> unit
val removeCheck: t -> value -> bool
val removeMany: t -> value array -> unit

val union: t -> t -> t
val intersect: t -> t -> t
val diff: t -> t -> t
val subset: t -> t -> bool

val cmp: t -> t -> int
val eq: t -> t -> bool


val forEachU: t -> (value -> unit [@bs]) ->  unit
val forEach: t -> (value -> unit ) ->  unit
(** In increasing order*)

val reduceU: t -> 'a -> ('a -> value -> 'a [@bs]) -> 'a
val reduce: t -> 'a -> ('a -> value -> 'a ) -> 'a
(** Iterate in increasing order. *)

val everyU: t -> (value -> bool [@bs]) -> bool
val every: t -> (value -> bool) ->  bool
(** `every p s` checks if all elements of the set
  satisfy the predicate `p`. Order unspecified. *)

val someU: t -> (value -> bool [@bs]) -> bool
val some: t -> (value -> bool) ->  bool
(** `some p s` checks if at least one element of
  the set satisfies the predicate `p`. Oder unspecified. *)

val keepU: t -> (value -> bool [@bs]) ->  t
val keep: t -> (value -> bool) ->  t
(** `keep s p` returns a fresh copy of the set of all elements in `s`
  that satisfy predicate `p`. *)

val partitionU: t -> (value -> bool [@bs]) ->  t * t
val partition: t -> (value -> bool) ->  t * t
(** `partition s p` returns a fresh copy pair of sets `(s1, s2)`, where
  `s1` is the set of all the elements of `s` that satisfy the
  predicate `p`, and `s2` is the set of all the elements of
  `s` that do not satisfy `p`. *)

val size: t -> int
val toList: t -> value list
(** In increasing order with respect *)

val toArray: t -> value array
(** In increasing order with respect *)

val minimum: t -> value option
val minUndefined: t -> value Js.undefined
val maximum: t -> value option
val maxUndefined: t -> value Js.undefined

val get:  t -> value -> value option
val getUndefined:  t -> value -> value Js.undefined
val getExn: t -> value -> value
val split:  t -> value  -> (t * t) * bool
(**
  `split s key` return a fresh copy of each
*)

val checkInvariantInternal: t -> unit
(**
  **raise** when invariant is not held
*)
