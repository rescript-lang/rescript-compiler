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

type ('key, 'value, 'id) t

type ('key, 'id) cmp = ('key, 'id) Belt_Id.cmp

val empty: ('k, 'v, 'id) t

val isEmpty: ('k, 'v, 'id) t -> bool

val has:
  ('k, 'a, 'id) t -> 'k ->
  cmp:('k, 'id) cmp ->
  bool

val cmpU:
    ('k, 'v, 'id) t ->
    ('k, 'v, 'id) t ->
    kcmp:('k, 'id) cmp ->
    vcmp:('v -> 'v -> int [@bs]) -> 
     int
val cmp: 
    ('k, 'v, 'id) t ->
    ('k, 'v, 'id) t ->
    kcmp:('k, 'id) cmp ->
    vcmp:('v -> 'v -> int) ->
     int

val eqU:
    ('k, 'a, 'id) t ->
    ('k, 'a, 'id) t ->
    kcmp:('k, 'id) cmp ->
    veq:('a -> 'a -> bool [@bs]) ->
    bool
val eq:
    ('k, 'a, 'id) t ->
    ('k, 'a, 'id) t ->
    kcmp:('k, 'id) cmp ->
    veq:('a -> 'a -> bool) ->
    bool
(** [eq m1 m2 cmp] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [cmp] is the equality predicate used to compare
    the data associated with the keys. *)

val forEachU: ('k, 'a, 'id) t -> ('k -> 'a -> unit [@bs]) -> unit
val forEach: ('k, 'a, 'id) t -> ('k -> 'a -> unit) -> unit
(** [forEach m f] applies [f] to all bindings in map [m].
    [f] receives the key as first argument, and the associated value
    as second argument. The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)

val reduceU: ('k, 'a, 'id) t -> 'b -> ('b -> 'k -> 'a -> 'b [@bs]) -> 'b
val reduce: ('k, 'a, 'id) t -> 'b -> ('b -> 'k -> 'a -> 'b) -> 'b
(** [reduce m a f] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m]
    (in increasing order), and [d1 ... dN] are the associated data. *)

val everyU: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) -> bool
val every: ('k, 'a, 'id) t -> ('k -> 'a -> bool) -> bool
(** [every m p] checks if all the bindings of the map
    satisfy the predicate [p]. Order unspecified *)

val someU: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) -> bool
val some: ('k, 'a, 'id) t -> ('k -> 'a -> bool) -> bool
(** [some m p] checks if at least one binding of the map
    satisfy the predicate [p]. Order unspecified *)

val size: ('k, 'a, 'id) t -> int

val toList: ('k, 'a, 'id) t -> ('k * 'a) list
(** In increasing order. *)

val toArray: ('k, 'a, 'id) t -> ('k * 'a) array


val fromArray: ('k * 'a) array -> cmp:('k,'id) cmp -> ('k,'a,'id) t

val keysToArray: ('k, 'a, 'id) t -> 'k array

val valuesToArray: ('k, 'a, 'id) t -> 'a array

val minKey: ('k, _, _) t -> 'k option

val minKeyUndefined: ('k, _, _) t -> 'k Js.undefined

val maxKey: ('k, _, _) t -> 'k option

val maxKeyUndefined: ('k, _, _) t -> 'k Js.undefined

val minimum: ('k, 'a, _) t -> ('k * 'a) option

val minUndefined: ('k, 'a, _) t -> ('k * 'a) Js.undefined

val maximum: ('k, 'a, _) t -> ('k * 'a) option

val maxUndefined:('k, 'a, _) t -> ('k * 'a) Js.undefined

val get:
  ('k, 'a, 'id) t -> 'k ->
  cmp:('k, 'id) cmp ->
  'a option

val getUndefined:
  ('k, 'a, 'id) t -> 'k ->
  cmp:('k, 'id) cmp ->
  'a Js.undefined

val getWithDefault:
    ('k, 'a, 'id) t -> 'k -> 'a ->
    cmp:('k, 'id) cmp ->
    'a

val getExn:
  ('k, 'a, 'id) t -> 'k ->
  cmp:('k, 'id) cmp ->
  'a

val checkInvariantInternal: _ t -> unit
(**
   {b raise} when invariant is not held
*)

val remove:
  ('a, 'b, 'id) t -> 'a ->
  cmp:('a, 'id) cmp ->
  ('a, 'b, 'id) t
(** [remove m x] returns a map containing the same bindings as
   [m], except for [x] which is unbound in the returned map. *)

val removeMany:
  ('a, 'b, 'id) t ->
  'a array ->
  cmp:('a, 'id) cmp ->
  ('a, 'b, 'id) t

val set:
  ('a, 'b, 'id) t -> 'a -> 'b ->
  cmp:('a, 'id) cmp ->
  ('a, 'b, 'id) t
(** [set m x y] returns a map containing the same bindings as
   [m], plus a binding of [x] to [y]. If [x] was already bound
   in [m], its previous binding disappears. *)

val updateU:
  ('a, 'b, 'id) t ->
  'a ->
  ('b option -> 'b option [@bs]) ->
  cmp:('a, 'id) cmp ->
  ('a, 'b, 'id) t
val update:
  ('a, 'b, 'id) t ->
  'a ->
  ('b option -> 'b option) ->
  cmp:('a, 'id) cmp ->
  ('a, 'b, 'id) t

val mergeU:
  ('a, 'b, 'id) t ->
  ('a, 'c, 'id) t ->
  ('a -> 'b option -> 'c option -> 'd option [@bs]) ->
  cmp:('a, 'id) cmp -> ('a, 'd, 'id) t
val merge:
  ('a, 'b, 'id) t ->
  ('a, 'c, 'id) t ->
  ('a -> 'b option -> 'c option -> 'd option) ->
  cmp:('a, 'id) cmp -> ('a, 'd, 'id) t
(** [merge m1 m2 f] computes a map whose keys is a subset of keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding
    value, is determined with the function [f].
 *)

val mergeMany:
  ('a, 'b, 'id) t ->
  ('a * 'b) array ->
  cmp:('a, 'id) cmp ->
  ('a, 'b, 'id) t

val keepU:
    ('k, 'a, 'id) t ->
    ('k -> 'a -> bool [@bs]) ->
    ('k, 'a, 'id) t
val keep:
    ('k, 'a, 'id) t ->
    ('k -> 'a -> bool) ->
    ('k, 'a, 'id) t
(** [keep m p] returns the map with all the bindings in [m]
    that satisfy predicate [p]. *)

val partitionU:
    ('k, 'a, 'id) t ->
    ('k -> 'a -> bool [@bs]) ->
    ('k, 'a, 'id) t * ('k, 'a, 'id) t
val partition:
    ('k, 'a, 'id) t ->
    ('k -> 'a -> bool) -> 
    ('k, 'a, 'id) t * ('k, 'a, 'id) t
(** [partition m p] returns a pair of maps [(m1, m2)], where
    [m1] contains all the bindings of [s] that satisfy the
    predicate [p], and [m2] is the map with all the bindings of
    [s] that do not satisfy [p].
*)

val split:
  ('a, 'b, 'id) t ->
  'a ->
  cmp:('a, 'id) cmp ->
  (('a,'b,'id) t * ('a, 'b, 'id) t) * 'b option
(** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose key
    is strictly less than [x];
      [r] is the map with all the bindings of [m] whose key
    is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
 *)

val mapU: ('k, 'a, 'id) t -> ('a -> 'b [@bs]) -> ('k ,'b,'id) t
val map: ('k, 'a, 'id) t -> ('a -> 'b) -> ('k ,'b,'id) t
(** [map m f] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapWithKeyU: ('k, 'a, 'id) t -> ('k -> 'a -> 'b [@bs]) -> ('k, 'b, 'id) t
val mapWithKey: ('k, 'a, 'id) t -> ('k -> 'a -> 'b) -> ('k, 'b, 'id) t
