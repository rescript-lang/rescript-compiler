# 1 "setm.cppo.mli"
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

# 26
type elt = string

# 33
type t
val empty: unit -> t
val singleton: elt -> t  
val ofArray: elt array -> t
val ofSortedArrayUnsafe: elt array -> t   
val isEmpty: t -> bool
val mem: t -> elt -> bool

val addDone: t -> elt -> unit
val add: t -> elt -> t
val addCheck: t -> elt -> bool 
val addArrayDone: t -> elt array -> unit
val addArray: t -> elt array -> t

val removeDone: t -> elt -> unit
val remove: t -> elt -> t
val removeCheck: t -> elt -> bool 
val removeArrayDone: t -> elt array -> unit
val removeArray: t -> elt array -> t
  
val union: t -> t -> t
val inter: t -> t -> t
val diff: t -> t -> t
val subset: t -> t -> bool
  
val cmp: t -> t -> int
val eq: t -> t -> bool



val iter: t -> (elt -> unit [@bs]) ->  unit
(** In increasing order*)

val fold: t -> 'a -> ('a -> elt ->  'a [@bs]) ->  'a
(** Iterate in increasing order. *)

val forAll: t -> (elt -> bool [@bs]) ->  bool
(** [for_all p s] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified. *)

val exists: t -> (elt -> bool [@bs]) ->  bool
(** [exists p s] checks if at least one element of
    the set satisfies the predicate [p]. Oder unspecified. *)

val filter: t -> (elt -> bool [@bs]) ->  t
(** [filter s p] returns a fresh copy of the set of all elements in [s]
    that satisfy predicate [p]. *)

val partition: t -> (elt -> bool [@bs]) ->  t * t
(** [partition s p] returns a fresh copy pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)

val length: t -> int
val toList: t -> elt list
 (** In increasing order with respect *)
val toArray: t -> elt array


val minimum: t -> elt option
val minNull: t -> elt Js.null
val maximum: t -> elt option
val maxNull: t -> elt Js.null

val get:  t -> elt -> elt option
val getNull:  t -> elt -> elt Js.null
val getExn: t -> elt -> elt
val split:  t -> elt  -> (t * t) * bool 
(**
    [split s key] return a fresh copy of each
*)

val checkInvariant: t ->  bool


