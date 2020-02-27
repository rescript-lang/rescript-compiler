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

(** A {b mutable} Hash set which allows customized {!hash} behavior.

    All data are parameterized by not its only type but also a unique identity in
    the time of initialization, so that two {i HashSets of ints} initialized with different
    {i hash} functions will have different type.

    For example:
    {[
      type t = int
      module I0 =
        (val Belt.Id.hashableU
            ~hash:(fun[\@bs] (a : t)  -> a & 0xff_ff)
            ~eq:(fun[\@bs] a b -> a = b)
        )
      let s0 = make ~id:(module I0) ~hintSize:40
      module I1 =
        (val Belt.Id.hashableU
            ~hash:(fun[\@bs] (a : t)  -> a & 0xff)
            ~eq:(fun[\@bs] a b -> a = b)
        )
      let s1 = make ~id:(module I1) ~hintSize:40
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
        add s1 0;
        add s1 1
    ]}

    Since this is an mutable data strucure, [s1] will contain two pairs.
*)


(** Specalized when key type is [int], more efficient
    than the generic type *)
module Int = Belt_HashSetInt

(** Specalized when key type is [string], more efficient
    than the generic type *)
module String = Belt_HashSetString

(* TODO: add a poly module
   module Poly = Belt_HashSetPoly
   challenge:
   - generic equal handles JS data structure
   - eq/hash consistent
*)

type ('a, 'id) t

(** The type of hash tables from type ['a] to type ['b]. *)

type ('a, 'id) id = ('a, 'id) Belt_Id.hashable

val make:  hintSize:int -> id:('a,'id) id ->  ('a, 'id) t (* [@@dead "make"] *)
val clear: ('a, 'id) t -> unit (* [@@dead "clear"] *)
val isEmpty: _ t -> bool (* [@@dead "isEmpty"] *)

val add: ('a, 'id) t -> 'a -> unit (* [@@dead "add"] *)

val copy: ('a, 'id) t -> ('a, 'id) t (* [@@dead "copy"] *)

val has: ('a, 'id) t -> 'a -> bool (* [@@dead "has"] *)

val remove: ('a, 'id) t -> 'a -> unit (* [@@dead "remove"] *)

val forEachU: ('a, 'id) t -> ('a  -> unit [@bs]) ->  unit (* [@@dead "forEachU"] *)
val forEach: ('a, 'id) t -> ('a  -> unit) ->  unit (* [@@dead "forEach"] *)
(** Order unspecified. *)

val reduceU: ('a, 'id) t -> 'c -> ('c -> 'a  ->  'c [@bs]) -> 'c (* [@@dead "reduceU"] *)
val reduce: ('a, 'id) t -> 'c -> ('c -> 'a  ->  'c) -> 'c (* [@@dead "reduce"] *)
(** Order unspecified. *)

val size: ('a, 'id) t -> int (* [@@dead "size"] *)

val logStats: _ t -> unit (* [@@dead "logStats"] *)

val toArray: ('a,'id) t -> 'a array (* [@@dead "toArray"] *)

val fromArray: 'a array -> id:('a,'id) id -> ('a,'id) t (* [@@dead "fromArray"] *)

val mergeMany: ('a,'id) t -> 'a array -> unit (* [@@dead "mergeMany"] *)

val getBucketHistogram: _ t -> int array (* [@@dead "getBucketHistogram"] *)

