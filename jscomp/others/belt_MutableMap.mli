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

module Int = Belt_MutableMapInt

module String = Belt_MutableMapString


(** A {b mutable} sorted map module which allows customize {i compare} behavior.

   Same as Belt.Map, but mutable.
*)

type ('k,'v,'id) t
type ('key, 'id) id = ('key, 'id) Belt_Id.comparable

val make: id:('k, 'id) id -> ('k, 'a, 'id) t (* [@@dead "make"] *)
val clear: _ t -> unit (* [@@dead "clear"] *)
val isEmpty: _ t -> bool
val has: ('k, _, _) t -> 'k  -> bool (* [@@dead "has"] *)

val cmpU: (* [@@dead "cmpU"] *)
    ('k, 'a, 'id) t ->
    ('k, 'a, 'id) t ->
    ('a -> 'a -> int [@bs]) ->
     int
val cmp: (* [@@dead "cmp"] *)
    ('k, 'a, 'id) t ->
    ('k, 'a, 'id) t ->
    ('a -> 'a -> int ) ->
     int
(** [cmp m1 m2 cmp]
    First compare by size, if size is the same,
    compare by key, value pair
*)

val eqU:  ('k, 'a, 'id) t -> ('k, 'a, 'id) t -> ('a -> 'a -> bool [@bs]) -> bool (* [@@dead "eqU"] *)
val eq:  ('k, 'a, 'id) t -> ('k, 'a, 'id) t -> ('a -> 'a -> bool) -> bool (* [@@dead "eq"] *)
(** [eq m1 m2 eqf] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [eqf] is the equality predicate used to compare
    the data associated with the keys. *)

val forEachU:  ('k, 'a, 'id) t -> ('k -> 'a -> unit [@bs]) -> unit (* [@@dead "forEachU"] *)
val forEach:  ('k, 'a, 'id) t -> ('k -> 'a -> unit) -> unit (* [@@dead "forEach"] *)
(** [forEach m f] applies [f] to all bindings in map [m].
    [f] receives the 'k as first argument, and the associated value
    as second argument.  The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)

val reduceU: ('k, 'a, 'id) t -> 'b ->  ('b -> 'k -> 'a -> 'b [@bs]) ->  'b (* [@@dead "reduceU"] *)
val reduce: ('k, 'a, 'id) t -> 'b ->  ('b -> 'k -> 'a -> 'b) ->  'b (* [@@dead "reduce"] *)
(** [reduce m a f] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m]
    (in increasing order), and [d1 ... dN] are the associated data. *)

val everyU: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) ->  bool (* [@@dead "everyU"] *)
val every: ('k, 'a, 'id) t -> ('k -> 'a -> bool) ->  bool (* [@@dead "every"] *)
(** [every m p] checks if all the bindings of the map
    satisfy the predicate [p].
*)


val someU: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) ->  bool (* [@@dead "someU"] *)
val some: ('k, 'a, 'id) t -> ('k -> 'a -> bool) ->  bool (* [@@dead "some"] *)
(** [some m p] checks if at least one binding of the map
    satisfy the predicate [p].
*)

val size: ('k, 'a, 'id) t -> int (* [@@dead "size"] *)


val toList: ('k, 'a, 'id) t -> ('k * 'a) list (* [@@dead "toList"] *)
(** In increasing order*)

val toArray: ('k, 'a, 'id) t -> ('k * 'a) array (* [@@dead "toArray"] *)
(** In increasing order*)

val fromArray: ('k * 'a) array -> id:('k,'id) id ->  ('k,'a,'id) t
val keysToArray: ('k, _, _) t -> 'k array
val valuesToArray: (_, 'a, _) t -> 'a array (* [@@dead "valuesToArray"] *)
val minKey: ('k, _,  _) t -> 'k option (* [@@dead "minKey"] *)
val minKeyUndefined: ('k, _,  _) t -> 'k Js.undefined (* [@@dead "minKeyUndefined"] *)
val maxKey: ('k, _,  _) t -> 'k option (* [@@dead "maxKey"] *)
val maxKeyUndefined: ('k, _,  _) t -> 'k Js.undefined (* [@@dead "maxKeyUndefined"] *)
val minimum: ('k, 'a,  _) t -> ('k * 'a) option (* [@@dead "minimum"] *)
val minUndefined: ('k, 'a, _) t -> ('k * 'a) Js.undefined (* [@@dead "minUndefined"] *)
val maximum: ('k, 'a, _) t -> ('k * 'a) option (* [@@dead "maximum"] *)
val maxUndefined:('k, 'a, _) t -> ('k * 'a) Js.undefined (* [@@dead "maxUndefined"] *)
val get:  ('k, 'a, 'id) t -> 'k -> 'a option (* [@@dead "get"] *)
val getUndefined: ('k, 'a, 'id) t -> 'k ->  'a Js.undefined (* [@@dead "getUndefined"] *)
val getWithDefault: (* [@@dead "getWithDefault"] *)
    ('k, 'a, 'id) t -> 'k ->  'a -> 'a
val getExn:  ('k, 'a, 'id) t -> 'k ->  'a
val checkInvariantInternal: _ t -> unit (* [@@dead "checkInvariantInternal"] *)
(**
   {b raise} when invariant is not held
*)


(****************************************************************************)

(*TODO: add functional [merge, partition, keep, split]*)

val remove:  ('k, 'a, 'id) t -> 'k -> unit (* [@@dead "remove"] *)
(** [remove m x] do the in-place modification,
*)

val removeMany: ('k, 'a, 'id) t -> 'k array -> unit

val set: ('k, 'a, 'id) t -> 'k -> 'a ->  unit
(** [set m x y ] do the in-place modification *)

val updateU: ('k, 'a, 'id) t -> 'k -> ('a option -> 'a option [@bs]) -> unit (* [@@dead "updateU"] *)
val update: ('k, 'a, 'id) t -> 'k -> ('a option -> 'a option) -> unit (* [@@dead "update"] *)

val mergeMany:  ('k, 'a, 'id) t -> ('k * 'a) array ->  unit (* [@@dead "mergeMany"] *)

val mapU: ('k, 'a, 'id) t -> ('a -> 'b [@bs]) ->  ('k ,'b,'id ) t (* [@@dead "mapU"] *)
val map: ('k, 'a, 'id) t -> ('a -> 'b) ->  ('k ,'b,'id ) t (* [@@dead "map"] *)
(** [map m f] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapWithKeyU: ('k, 'a, 'id) t -> ('k -> 'a -> 'b [@bs]) -> ('k, 'b, 'id) t (* [@@dead "mapWithKeyU"] *)
val mapWithKey: ('k, 'a, 'id) t -> ('k -> 'a -> 'b) -> ('k, 'b, 'id) t (* [@@dead "mapWithKey"] *)

