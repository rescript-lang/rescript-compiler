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

(** A {i immutable} sorted set module which allows customize {i compare} behavior.

   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.

  For more info on this module's usage of identity, `make` and others, please see
  the top level documentation of Belt, {b A special encoding for collection safety}.

  Example usage:

   @example {[
    module PairComparator = Belt.Id.MakeComparable(struct
      type t = int * int
      let cmp (a0, a1) (b0, b1) =
        match Pervasives.compare a0 b0 with
        | 0 -> Pervasives.compare a1 b1
        | c -> c
    end)

    let mySet = Belt.Set.make ~id:(module PairComparator)
    let mySet2 = Belt.Set.add mySet (1, 2)
  ]}

  The API documentation below will assume a predeclared comparator module for integers, IntCmp
*)

(** Specalized when value type is [int], more efficient
    than the generic type, its compare behavior is fixed using the built-in comparison
*)
module Int = Belt_SetInt

(** Specalized when value type is [string], more efficient
    than the generic type, its compare behavior is fixed using the built-in comparison
*)
module String = Belt_SetString


(** This module seprate identity from data, it is a bit more verboe but slightly
    more efficient due to the fact that there is no need to pack identity and data back
    after each operation
*)
module Dict = Belt_SetDict


type ('value, 'identity) t
(** [('value, 'identity) t]

    ['value] is the element type

    ['identity] the identity of the collection
*)


type ('value, 'id) id = ('value, 'id) Belt_Id.comparable
(** The identity needed for making a set from scratch
*)

val make: id:('value, 'id) id -> ('value, 'id) t
(** [make ~id] creates a new set by taking in the comparator
    @example {[
      let s = make ~id:(module IntCmp)
    ]}

*)



val fromArray:  'value array -> id:('value, 'id) id ->  ('value, 'id) t
(** [fromArray xs ~id]

    @example{[
     toArray (fromArray [1;3;2;4] (module IntCmp)) = [1;2;3;4]
    ]}
*)



val fromSortedArrayUnsafe: 'value array -> id:('value, 'id) id -> ('value,'id) t
(** [fromSortedArrayUnsafe xs ~id]

    The same as {!fromArray} except it is after assuming the input array [x] is already sorted

    {b Unsafe}
*)


val isEmpty: _ t -> bool
(**
   @example {[
     isEmpty (fromArray [||] ~id:(module IntCmp)) = true;;
     isEmpty (fromArray [|1|] ~id:(module IntCmp)) = true;;
   ]}
*)
val has: ('value, 'id) t -> 'value ->  bool
(**
   @example {[
     let v = fromArray [|1;4;2;5|] ~id:(module IntCmp);;
     has v 3 = false;;
     has v 1 = true;;
   ]}
*)

val add:
  ('value, 'id) t -> 'value -> ('value, 'id) t
(** [add s x] If [x] was already in [s], [s] is returned unchanged.

    @example {[
     let s0 = make ~id:(module IntCmp);;
     let s1 = add s0 1 ;;
     let s2 = add s1 2;;
     let s3 = add s2 2;;
     toArray s0 = [||];;
     toArray s1 = [|1|];;
     toArray s2 = [|1;2|];;
     toArray s3 = [|1;2|];;
     s2 == s3;;
    ]}
*)

val mergeMany: ('value, 'id) t -> 'value array -> ('value, 'id) t
(** [mergeMany s xs]

    Adding each of [xs] to [s], note unlike {!add},
    the reference of return value might be changed even if all values in [xs]
    exist [s]

*)
val remove: ('value, 'id) t -> 'value -> ('value, 'id) t
(** [remove m x] If [x] was not in [m], [m] is returned reference unchanged.

    @example {[
      let s0 = fromArray ~id:(module IntCmp) [|2;3;1;4;5|];;
      let s1 = remove s0 1 ;;
      let s2 = remove s1 3 ;;
      let s3 = remove s2 3 ;;

      toArray s1 = [|2;3;4;5|];;
      toArray s2 = [|2;4;5|];;
      s2 == s3;;
    ]}
*)

val removeMany:
  ('value, 'id) t -> 'value array -> ('value, 'id) t
(** [removeMany s xs]

    Removing each of [xs] to [s], note unlike {!remove},
    the reference of return value might be changed even if none in [xs]
    exists [s]
*)

val union: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
(**
   [union s0 s1]

   @example {[
     let s0 = fromArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
     let s1 = fromArray ~id:(module IntCmp) [|5;2;3;1;5;4;|];;
     toArray (union s0 s1) =  [|1;2;3;4;5;6|]
   ]}
*)

val intersect: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
(** [intersect s0 s1]
   @example {[
     let s0 = fromArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
     let s1 = fromArray ~id:(module IntCmp) [|5;2;3;1;5;4;|];;
     toArray (intersect s0 s1) =  [|2;3;5|]
   ]}

*)
val diff: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
(** [diff s0 s1]
    @example {[
      let s0 = fromArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      let s1 = fromArray ~id:(module IntCmp) [|5;2;3;1;5;4;|];;
      toArray (diff s0 s1) = [|6|];;
      toArray (diff s1 s0) = [|1;4|];;
    ]}
*)

val subset: ('value, 'id) t -> ('value, 'id) t -> bool
(** [subset s0 s1]

    @example {[
      let s0 = fromArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      let s1 = fromArray ~id:(module IntCmp) [|5;2;3;1;5;4;|];;
      let s2 = intersect s0 s1;;
      subset s2 s0 = true;;
      subset s2 s1 = true;;
      subset s1 s0 = false;;
    ]}
*)

val cmp: ('value, 'id) t -> ('value, 'id) t -> int
(** Total ordering between sets. Can be used as the ordering function
    for doing sets of sets.
    It compare [size] first and then iterate over
    each element following the order of elements
*)

val eq: ('value, 'id) t -> ('value, 'id) t -> bool
(** [eq s0 s1]

    @return true if [toArray s0 = toArray s1]
*)

val forEachU: ('value, 'id) t -> ('value -> unit [@bs]) ->  unit
val forEach: ('value, 'id) t -> ('value -> unit ) ->  unit
(** [forEach s f] applies [f] in turn to all elements of [s].
    In increasing order

    @example {[
      let s0 = fromArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      let acc = ref [] ;;
      forEach s0 (fun x -> acc := x !acc);;
      !acc = [6;5;3;2];;
    ]}
*)

val reduceU: ('value, 'id) t -> 'a  -> ('a -> 'value -> 'a [@bs]) ->  'a
val reduce: ('value, 'id) t -> 'a  -> ('a -> 'value -> 'a ) ->  'a
(** In increasing order.

    @example {[
      let s0 = fromArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      reduce s0 [] Bs.List.add = [6;5;3;2];;
    ]}
*)

val everyU: ('value, 'id) t -> ('value -> bool [@bs]) -> bool
val every: ('value, 'id) t -> ('value -> bool ) -> bool
(** [every p s] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified.
*)

val someU: ('value, 'id) t ->  ('value -> bool [@bs]) -> bool
val some: ('value, 'id) t ->  ('value -> bool ) -> bool
(** [some p s] checks if at least one element of
    the set satisfies the predicate [p]. *)

val keepU: ('value, 'id) t ->  ('value -> bool [@bs]) -> ('value, 'id) t
val keep: ('value, 'id) t ->  ('value -> bool ) -> ('value, 'id) t
(** [keep m p] returns the set of all elements in [s]
    that satisfy predicate [p]. *)

val partitionU: ('value, 'id) t -> ('value -> bool [@bs]) ->  ('value, 'id) t * ('value, 'id) t
val partition: ('value, 'id) t -> ('value -> bool) ->  ('value, 'id) t * ('value, 'id) t
(** [partition m p] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)

val size:  ('value, 'id) t -> int
(** [size s]

    @example {[
      let s0 = fromArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      size s0 = 4;;
    ]}
*)

val toArray: ('value, 'id) t -> 'value array
(** [toArray s0]
   @example {[
      let s0 = fromArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      toArray s0 = [|2;3;5;6|];;
    ]}*)

val toList: ('value, 'id) t -> 'value list
(** In increasing order

    {b See} {!toArray}
*)

val minimum: ('value, 'id) t -> 'value option
(** [minimum s0]

    @return the minimum element of the collection, [None] if it is empty
*)

val minUndefined: ('value, 'id) t -> 'value Js.undefined
(** [minUndefined s0]

   @return the minimum element of the collection, [undefined] if it is empty
*)

val maximum: ('value, 'id) t -> 'value option
(** [maximum s0]

    @return the maximum element of the collection, [None] if it is empty
*)
val maxUndefined: ('value, 'id) t -> 'value Js.undefined
(** [maxUndefined s0]

    @return the maximum element of the collection, [undefined] if it is empty
*)

val get: ('value, 'id) t -> 'value -> 'value option
(** [get s0 k]

    @return the reference of the value [k'] which is equivalent to [k]
    using  the comparator specifiecd by this collection, [None]
    if it does not exist
*)

val getUndefined: ('value, 'id) t -> 'value -> 'value Js.undefined
(** {b See} {!get}
    *)

val getExn: ('value, 'id) t -> 'value -> 'value
(** {b See} {!get}

    {b raise} if not exist
*)

val split: ('value, 'id) t -> 'value -> (('value, 'id) t  * ('value, 'id) t) * bool
(** [split set ele]

    @return  a tuple [((smaller, larger), present)],
    [present] is true when [ele] exist in [set]
*)

(**/**)
val checkInvariantInternal: _ t -> unit
(**
   {b raise} when invariant is not held
*)
(**/**)

(****************************************************************************)
(** Below are operations only when better performance needed,
    it is still safe API but more verbose.
    More API will be exposed by needs
*)

val getData: ('value, 'id) t  -> ('value, 'id) Belt_SetDict.t
(** [getData s0]

    {b Advanced usage only}

    @return the raw data (detached from comparator),
    but its type is still manifested, so that user can pass identity directly
    without boxing
*)

val getId: ('value, 'id) t  -> ('value, 'id) id
(** [getId s0]

    {b Advanced usage only}

    @return the identity of [s0]
*)

val packIdData: id:('value, 'id) id -> data:('value, 'id) Belt_SetDict.t -> ('value, 'id) t
(** [packIdData ~id ~data]

    {b Advanced usage only}

    @return the packed collection
*)

