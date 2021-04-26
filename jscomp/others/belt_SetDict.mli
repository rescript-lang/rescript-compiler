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

type ('value, 'identity) t

type ('value, 'id) cmp = ('value, 'id) Belt_Id.cmp

val empty: ('value, 'id) t


val fromArray: 'value array -> cmp:('value, 'id) cmp -> ('value, 'id) t

val fromSortedArrayUnsafe: 'value array -> ('value,'id) t

val isEmpty: _ t -> bool

val has: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> bool

val add: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> ('value, 'id) t
(** `add s x` If `x` was already in `s`, `s` is returned unchanged. *)

val mergeMany: ('value, 'id) t -> 'value array -> cmp:('value, 'id) cmp -> ('value, 'id) t

val remove: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> ('value, 'id) t
(** `remove m x` If `x` was not in `m`, `m` is returned reference unchanged. *)

val removeMany: ('value, 'id) t -> 'value array -> cmp:('value, 'id) cmp -> ('value, 'id) t

val union: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> ('value, 'id) t

val intersect: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> ('value, 'id) t

val diff: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> ('value, 'id) t

val subset: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> bool
(** `subset s1 s2` tests whether the set `s1` is a subset of
  the set `s2`. *)

val cmp: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> int
(** Total ordering between sets. Can be used as the ordering function
  for doing sets of sets. *)

val eq: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> bool
(** `eq s1 s2` tests whether the sets `s1` and `s2` are
  equal, that is, contain equal elements. *)

val forEachU: ('value, 'id) t -> ('value -> unit [@bs]) -> unit
val forEach: ('value, 'id) t -> ('value -> unit) -> unit
(** `forEach s f` applies `f` in turn to all elements of `s`.
  In increasing order *)

val reduceU: ('value, 'id) t -> 'a -> ('a -> 'value -> 'a [@bs]) -> 'a
val reduce: ('value, 'id) t -> 'a -> ('a -> 'value -> 'a) -> 'a
(** Iterate in increasing order. *)

val everyU: ('value, 'id) t -> ('value -> bool [@bs]) -> bool
val every: ('value, 'id) t -> ('value -> bool) -> bool
(** `every p s` checks if all elements of the set
  satisfy the predicate `p`. Order unspecified. *)

val someU: ('value, 'id) t -> ('value -> bool [@bs]) -> bool
val some: ('value, 'id) t -> ('value -> bool) -> bool
(** `some p s` checks if at least one element of
  the set satisfies the predicate `p`. Oder unspecified. *)

val keepU: ('value, 'id) t -> ('value -> bool [@bs]) -> ('value, 'id) t
val keep: ('value, 'id) t -> ('value -> bool) -> ('value, 'id) t
(** `keep p s` returns the set of all elements in `s`
  that satisfy predicate `p`. *)

val partitionU: ('value, 'id) t -> ('value -> bool [@bs]) -> ('value, 'id) t * ('value, 'id) t
val partition: ('value, 'id) t -> ('value -> bool) -> ('value, 'id) t * ('value, 'id) t
(**
  `partition p s` returns a pair of sets `(s1, s2)`, where
  `s1` is the set of all the elements of `s` that satisfy the
  predicate `p`, and `s2` is the set of all the elements of
  `s` that do not satisfy `p`.
*)

val size: ('value, 'id) t -> int

val toList: ('value, 'id) t -> 'value list
(** In increasing order *)

val toArray: ('value, 'id) t -> 'value array

val minimum: ('value, 'id) t -> 'value option

val minUndefined: ('value, 'id) t -> 'value Js.undefined

val maximum: ('value, 'id) t -> 'value option

val maxUndefined: ('value, 'id) t -> 'value Js.undefined

val get: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> 'value option

val getUndefined: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> 'value Js.undefined

val getExn: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> 'value

val split: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> (('value, 'id) t * ('value, 'id) t) * bool
(** `split x s` returns a triple `(l, present, r)`, where
  `l` is the set of elements of `s` that are
  strictly less than `x`;
  `r` is the set of elements of `s` that are
  strictly greater than `x`;
  `present` is `false` if `s` contains no element equal to `x`,
  or `true` if `s` contains an element equal to `x`.
*)

val checkInvariantInternal: _ t -> unit
(**
  **raise** when invariant is not held
*)
