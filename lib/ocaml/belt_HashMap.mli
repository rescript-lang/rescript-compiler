(* Copyright (C) 2018 Authors of BuckleScript
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

(** A {b mutable} Hash map which allows customized {!hash} behavior.

    All data are parameterized by not its only type but also a unique identity in
    the time of initialization, so that two {i HashMaps of ints} initialized with different
    {i hash} functions will have different type.

    For example:
    {[
      type t = int
      module I0 =
        (val Belt.Id.hashableU
            ~hash:(fun[\@bs] (a : t)  -> a & 0xff_ff)
            ~eq:(fun[\@bs] a b -> a = b)
        )
      let s0 : (_, string,_) t = make ~hintSize:40 ~id:(module I0)
      module I1 =
        (val Belt.Id.hashableU
            ~hash:(fun[\@bs] (a : t)  -> a & 0xff)
            ~eq:(fun[\@bs] a b -> a = b)
        )
      let s1 : (_, string,_) t  = make ~hintSize:40 ~id:(module I1)
    ]}

    The invariant must be held: for two elements who are {i equal},
    their hashed value should be the same

    Here the compiler would infer [s0] and [s1] having different type so that
    it would not mix.

    {[
      val s0 :  (int, I0.identity) t
      val s1 :  (int, I1.identity) t
    ]}

    We can add elements to the collection:

    {[

      let () =
        add s1 0 "3";
        add s1 1 "3"
    ]}

    Since this is an mutable data strucure, [s1] will contain two pairs.
*)


(** Specalized when key type is [int], more efficient
    than the generic type *)
module Int = Belt_HashMapInt


(** Specalized when key type is [string], more efficient
    than the generic type *)
module String = Belt_HashMapString




type ('key,'value,'id) t
(** The type of hash tables from type ['key] to type ['value]. *)

type ('a, 'id) id = ('a, 'id) Belt_Id.hashable


val make:  hintSize:int -> id:('key, 'id) id -> ('key,'value,'id) t
(*TODO: allow randomization for security *)

val clear: ('key, 'value, 'id ) t -> unit
(** Empty a hash table. *)

val isEmpty: _ t -> bool

val set: ('key, 'value, 'id ) t -> 'key -> 'value -> unit
(** [set tbl k v] if [k] does not exist,
     add the binding [k,v], otherwise, update the old value with the new
     [v]
*)

val copy: ('key, 'value, 'id ) t -> ('key, 'value, 'id ) t

val get: ('key, 'value, 'id ) t -> 'key -> 'value option


val has: ('key, 'value, 'id ) t -> 'key -> bool
(** [has tbl x] checks if [x] is bound in [tbl]. *)

val remove: ('key, 'value, 'id ) t -> 'key ->  unit

val forEachU: ('key, 'value, 'id ) t -> ('key -> 'value -> unit [@bs]) -> unit
val forEach: ('key, 'value, 'id ) t -> ('key -> 'value -> unit) -> unit
(** [forEach tbl f] applies [f] to all bindings in table [tbl].
    [f] receives the key as first argument, and the associated value
    as second argument. Each binding is presented exactly once to [f].
*)

val reduceU: ('key, 'value, 'id ) t -> 'c -> ('c -> 'key -> 'value ->  'c [@bs]) ->  'c
val reduce: ('key, 'value, 'id ) t -> 'c -> ('c -> 'key -> 'value ->  'c) ->  'c
(** [reduce  tbl init f] computes
    [(f kN dN ... (f k1 d1 init)...)],
    where [k1 ... kN] are the keys of all bindings in [tbl],
    and [d1 ... dN] are the associated values.
    Each binding is presented exactly once to [f].

    The order in which the bindings are passed to [f] is unspecified.
    However, if the table contains several bindings for the same key,
    they are passed to [f] in reverse order of introduction, that is,
    the most recent binding is passed first.
*)


val keepMapInPlaceU: ('key, 'value, 'id ) t -> ('key -> 'value -> 'value option [@bs]) ->  unit
val keepMapInPlace: ('key, 'value, 'id ) t -> ('key -> 'value -> 'value option ) ->  unit


val size: _ t -> int
(** [size tbl] returns the number of bindings in [tbl].
    It takes constant time. *)






val toArray: ('key, 'value, 'id ) t -> ('key * 'value) array
val keysToArray: ('key, _, _) t -> 'key array
val valuesToArray: (_,'value,_) t -> 'value array
val fromArray: ('key * 'value) array -> id:('key,'id) id -> ('key, 'value, 'id ) t
val mergeMany: ('key, 'value, 'id ) t -> ('key * 'value) array -> unit
val getBucketHistogram: _ t -> int array
val logStats: _ t -> unit


