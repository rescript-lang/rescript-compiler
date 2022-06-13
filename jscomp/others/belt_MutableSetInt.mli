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

  This module is [Belt.MutableSet](mutable-set) specialized with key type to be a `int` type.
  It is more efficient in general, the API is the same with [Belt.MutableSet](mutable-set) except its key type is fixed, and identity is not needed (using the built-in one).
*)


# 38 "others/setm.cppo.mli"
type value = int

# 42 "others/setm.cppo.mli"
  (** The type of the set elements. *)


type t
(**
  The type of sets.
*)

val make: unit -> t
(**
  Returns empty set.

  ```res example
  let set = Belt.MutableSet.Int.make()
  ```
*)

val fromArray: value array -> t
(**
  Creates new set from array of elements.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([1, 3, 2, 4])

  s0->Belt.MutableSet.Int.toArray /* [1, 2, 3, 4] */
  ```
*)

val fromSortedArrayUnsafe: value array -> t
(**
  The same as [fromArray][#fromarray] except it is after assuming the input array is already sorted.
*)

val copy: t -> t
(**
  Returns copy of a set.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([1, 3, 2, 4])

  let copied = s0->Belt.MutableSet.Int.copy
  copied->Belt.MutableSet.Int.toArray /* [1, 2, 3, 4] */
  ```
*)

val isEmpty: t -> bool
(**
  Checks if set is empty.

  ```res example
  let empty = Belt.MutableSet.Int.fromArray([])
  let notEmpty = Belt.MutableSet.Int.fromArray([1])

  Belt.MutableSet.Int.isEmpty(empty) /* true */
  Belt.MutableSet.Int.isEmpty(notEmpty) /* false */
  ```
*)

val has: t -> value -> bool
(**
  Checks if element exists in set.

  ```res example
  let set = Belt.MutableSet.Int.fromArray([1, 4, 2, 5])

  set->Belt.MutableSet.Int.has(3) /* false */
  set->Belt.MutableSet.Int.has(1) /* true */
  ```
*)

val add: t -> value -> unit
(**
  Adds element to set. If element existed in set, value is unchanged.

  ```res example
  let s0 = Belt.MutableSet.Int.make()
  s0->Belt.MutableSet.Int.add(1)
  s0->Belt.MutableSet.Int.add(2)
  s0->Belt.MutableSet.Int.add(2)

  s0->Belt.MutableSet.Int.toArray /* [1, 2] */
  ```
*)

val addCheck: t -> value -> bool

val mergeMany: t -> value array -> unit
(**
  Adds each element of array to set. Unlike [add](#add), the reference of return value might be changed even if all values in array already exist in set

  ```res example
  let set = Belt.MutableSet.Int.make()

  set->Belt.MutableSet.Int.mergeMany([5, 4, 3, 2, 1])
  set->Belt.MutableSet.Int.toArray /* [1, 2, 3, 4, 5] */
  ```
*)

val remove: t -> value -> unit
(**
  Removes element from set. If element wasn't existed in set, value is unchanged.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([2, 3, 1, 4, 5])
  s0->Belt.MutableSet.Int.remove(1)
  s0->Belt.MutableSet.Int.remove(3)
  s0->Belt.MutableSet.Int.remove(3)

  s0->Belt.MutableSet.Int.toArray /* [2,4,5] */
  ```
*)

val removeCheck: t -> value -> bool

val removeMany: t -> value array -> unit
(**
  Removes each element of array from set.

  ```res example
  let set = Belt.MutableSet.Int.fromArray([1, 2, 3, 4])

  set->Belt.MutableSet.Int.removeMany([5, 4, 3, 2, 1])
  set->Belt.MutableSet.Int.toArray /* [] */
  ```
*)

val union: t -> t -> t
(**
  Returns union of two sets.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([5, 2, 3, 5, 6])
  let s1 = Belt.MutableSet.Int.fromArray([5, 2, 3, 1, 5, 4])
  let union = Belt.MutableSet.Int.union(s0, s1)
  union->Belt.MutableSet.Int.toArray /* [1,2,3,4,5,6] */
  ```
*)

val intersect: t -> t -> t
(**
  Returns intersection of two sets.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([5, 2, 3, 5, 6])
  let s1 = Belt.MutableSet.Int.fromArray([5, 2, 3, 1, 5, 4])
  let intersect = Belt.MutableSet.Int.intersect(s0, s1)
  intersect->Belt.MutableSet.Int.toArray /* [2,3,5] */
  ```
*)

val diff: t -> t -> t
(**
  Returns elements from first set, not existing in second set.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([5, 2, 3, 5, 6])
  let s1 = Belt.MutableSet.Int.fromArray([5, 2, 3, 1, 5, 4])
  Belt.MutableSet.Int.toArray(Belt.MutableSet.Int.diff(s0, s1)) /* [6] */
  Belt.MutableSet.Int.toArray(Belt.MutableSet.Int.diff(s1, s0)) /* [1,4] */
  ```
*)

val subset: t -> t -> bool
(**
  Checks if second set is subset of first set.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([5, 2, 3, 5, 6])
  let s1 = Belt.MutableSet.Int.fromArray([5, 2, 3, 1, 5, 4])
  let s2 = Belt.MutableSet.Int.intersect(s0, s1)
  Belt.MutableSet.Int.subset(s2, s0) /* true */
  Belt.MutableSet.Int.subset(s2, s1) /* true */
  Belt.MutableSet.Int.subset(s1, s0) /* false */
  ```
*)

val cmp: t -> t -> int
(**
  Total ordering between sets. Can be used as the ordering function for doing sets of sets. It compares size first and then iterates over each element following the order of elements.
*)

val eq: t -> t -> bool
(**
  Checks if two sets are equal.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([5, 2, 3])
  let s1 = Belt.MutableSet.Int.fromArray([3, 2, 5])

  Belt.MutableSet.Int.eq(s0, s1) /* true */
  ```
*)

val forEachU: t -> (value -> unit [@bs]) ->  unit
(**
  Same as [forEach](##forEach) but takes uncurried functon.
*)

val forEach: t -> (value -> unit ) ->  unit
(**
  Applies function `f` in turn to all elements of set in increasing order.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([5, 2, 3, 5, 6])
  let acc = ref(list{})
  s0->Belt.MutableSet.Int.forEach(x => acc := Belt.List.add(acc.contents, x))
  acc /* [6,5,3,2] */
  ```
*)

val reduceU: t -> 'a -> ('a -> value -> 'a [@bs]) -> 'a

val reduce: t -> 'a -> ('a -> value -> 'a ) -> 'a
(**
  Applies function `f` to each element of set in increasing order. Function `f` has two parameters: the item from the set and an “accumulator”, which starts with a value of `initialValue`. `reduce` returns the final value of the accumulator.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([5, 2, 3, 5, 6])
  s0->Belt.MutableSet.Int.reduce(list{}, (acc, element) =>
    acc->Belt.List.add(element)
  ) /* [6,5,3,2] */
  ```
*)

val everyU: t -> (value -> bool [@bs]) -> bool

val every: t -> (value -> bool) ->  bool
(**
  Checks if all elements of the set satisfy the predicate. Order unspecified.

  ```res example
  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.MutableSet.Int.fromArray([2, 4, 6, 8])
  s0->Belt.MutableSet.Int.every(isEven) /* true */
  ```
*)

val someU: t -> (value -> bool [@bs]) -> bool

val some: t -> (value -> bool) ->  bool
(**
  Checks if at least one element of the set satisfies the predicate.

  ```res example
  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.MutableSet.Int.fromArray([1, 2, 4, 6, 8])
  s0->Belt.MutableSet.Int.some(isOdd) /* true */
  ```
*)

val keepU: t -> (value -> bool [@bs]) ->  t

val keep: t -> (value -> bool) ->  t
(**
  Returns the set of all elements that satisfy the predicate.

  ```res example
  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.MutableSet.Int.fromArray([1, 2, 3, 4, 5])
  let s1 = s0->Belt.MutableSet.Int.keep(isEven)

  s1->Belt.MutableSet.Int.toArray /* [2, 4] */
  ```
*)

val partitionU: t -> (value -> bool [@bs]) ->  t * t

val partition: t -> (value -> bool) ->  t * t
(**
  ```res example
  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.MutableSet.Int.fromArray([1, 2, 3, 4, 5])
  let (s1, s2) = s0->Belt.MutableSet.Int.partition(isOdd)

  s1->Belt.MutableSet.Int.toArray /* [1,3,5] */
  s2->Belt.MutableSet.Int.toArray /* [2,4] */
  ```
*)

val size: t -> int
(**
  Returns size of the set.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([1, 2, 3, 4])

  s0->Belt.MutableSet.Int.size /* 4 */
  ```
*)

val toList: t -> value list
(**
  Returns list of ordered set elements.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([3, 2, 1, 5])

  s0->Belt.MutableSet.Int.toList /* [1,2,3,5] */
  ```
*)

val toArray: t -> value array
(**
  Returns array of ordered set elements.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([3, 2, 1, 5])

  s0->Belt.MutableSet.Int.toArray /* [1,2,3,5] */
  ```
*)

val minimum: t -> value option
(**
  Returns minimum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.MutableSet.Int.make()
  let s1 = Belt.MutableSet.Int.fromArray([3, 2, 1, 5])

  s0->Belt.MutableSet.Int.minimum /* None */
  s1->Belt.MutableSet.Int.minimum /* Some(1) */
  ```
*)

val minUndefined: t -> value Js.undefined
(**
  Returns minimum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.MutableSet.Int.make()
  let s1 = Belt.MutableSet.Int.fromArray([3, 2, 1, 5])

  s0->Belt.MutableSet.Int.minUndefined /* undefined */
  s1->Belt.MutableSet.Int.minUndefined /* 1 */
  ```
*)

val maximum: t -> value option
(**
  Returns maximum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.MutableSet.Int.make()
  let s1 = Belt.MutableSet.Int.fromArray([3, 2, 1, 5])

  s0->Belt.MutableSet.Int.maximum /* None */
  s1->Belt.MutableSet.Int.maximum /* Some(5) */
  ```
*)

val maxUndefined: t -> value Js.undefined
(**
  Returns maximum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.MutableSet.Int.make()
  let s1 = Belt.MutableSet.Int.fromArray([3, 2, 1, 5])

  s0->Belt.MutableSet.Int.maxUndefined /* undefined */
  s1->Belt.MutableSet.Int.maxUndefined /* 5 */
  ```
*)

val get:  t -> value -> value option
(**
  Returns the reference of the value which is equivalent to value using the comparator specifiecd by this collection. Returns `None` if element does not exist.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([1, 2, 3, 4, 5])

  s0->Belt.MutableSet.Int.get(3) /* Some(3) */
  s0->Belt.MutableSet.Int.get(20) /* None */
  ```
*)

val getUndefined:  t -> value -> value Js.undefined
(**
  Same as [get](#get) but returns `undefined` when element does not exist.
*)

val getExn: t -> value -> value
(**
  Same as [get](#get) but raise when element does not exist.
*)

val split:  t -> value  -> (t * t) * bool
(**
  Returns a tuple `((smaller, larger), present)`, `present` is true when element exist in set.

  ```res example
  let s0 = Belt.MutableSet.Int.fromArray([1, 2, 3, 4, 5])

  let ((smaller, larger), present) = s0->Belt.MutableSet.Int.split(3)

  present /* true */
  smaller->Belt.MutableSet.Int.toArray /* [1,2] */
  larger->Belt.MutableSet.Int.toArray /* [4,5] */
  ```
*)

val checkInvariantInternal: t -> unit
(**
  **raise** when invariant is not held
*)
