# 1 "others/belt_Set.cppo.mli"
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
  This module is [`Belt.Set`]() specialized with value type to be a primitive type.
  It is more efficient in general, the  API is the same with [`Belt_Set`]() except its value type is fixed,
  and identity is not needed(using the built-in one)

  Specalized when value type is `int`, more efficient than the generic type, its compare behavior is fixed using the built-in comparison.

  **See** [`Belt.Set`]()
*)

# 36 "others/belt_Set.cppo.mli"
type value = int
(**
  ```res prelude
  type value = int
  ```
*)

# 40 "others/belt_Set.cppo.mli"
  (** The type of the set elements. *)


type t
(**
  ```res prelude
  type t
  ```

  Type of the sets.
*)

val empty: t
(**
  ```res sig
  let empty: t
  ```

  Empty set

  ```res example
  let s0 = Belt.Set.Int.empty
  ```
*)

val fromArray: value array -> t
(**
  ```res sig
  let fromArray: array<value> => t
  ```

  Creates new set from array of elements.

  ```res example
  let s0 = Belt.Set.Int.fromArray([1, 3, 2, 4])

  s0->Belt.Set.Int.toArray /* [1, 2, 3, 4] */
  ```
*)

val fromSortedArrayUnsafe: value array -> t
(**
  ```res sig
  let fromSortedArrayUnsafe: array<value> => t
  ```

  The same as [fromArray][#fromarray] except it is after assuming the input array is already sorted.
*)

val isEmpty: t -> bool
(**
  ```res sig
  let isEmpty: t => bool
  ```

  Checks if set is empty.

  ```res example
  let empty = Belt.Set.Int.fromArray([])
  let notEmpty = Belt.Set.Int.fromArray([1])

  Belt.Set.Int.isEmpty(empty) /* true */
  Belt.Set.Int.isEmpty(notEmpty) /* false */
  ```
*)

val has: t -> value -> bool
(**
  ```res sig
  let has: (t, value) => bool
  ```

  Checks if element exists in set.

  ```res example
  let set = Belt.Set.Int.fromArray([1, 4, 2, 5])

  set->Belt.Set.Int.has(3) /* false */
  set->Belt.Set.Int.has(1) /* true */
  ```
*)

val add: t -> value -> t
(**
  ```res sig
  let add: (t, value) => t
  ```

  Adds element to set. If element existed in set, value is unchanged.

  ```res example
  let s0 = Belt.Set.Int.empty
  let s1 = s0->Belt.Set.Int.add(1)
  let s2 = s1->Belt.Set.Int.add(2)
  let s3 = s2->Belt.Set.Int.add(2)
  s0->Belt.Set.Int.toArray /* [] */
  s1->Belt.Set.Int.toArray /* [1] */
  s2->Belt.Set.Int.toArray /* [1, 2] */
  s3->Belt.Set.Int.toArray /* [1,2 ] */
  s2 == s3 /* true */
  ```
*)

val mergeMany: t -> value array -> t
(**
  ```res sig
  let mergeMany: (t, array<value>) => t
  ```

  Adds each element of array to set. Unlike [add](#add), the reference of return value might be changed even if all values in array already exist in set

  ```res example
  let set = Belt.Set.Int.empty

  let newSet = set->Belt.Set.Int.mergeMany([5, 4, 3, 2, 1])
  newSet->Belt.Set.Int.toArray /* [1, 2, 3, 4, 5] */
  ```
*)

val remove: t -> value -> t
(**
  ```res sig
  let remove: (t, value) => t
  ```

  Removes element from set. If element wasn't existed in set, value is unchanged.

  ```res example
  let s0 = Belt.Set.Int.fromArray([2, 3, 1, 4, 5])
  let s1 = s0->Belt.Set.Int.remove(1)
  let s2 = s1->Belt.Set.Int.remove(3)
  let s3 = s2->Belt.Set.Int.remove(3)

  s1->Belt.Set.Int.toArray /* [2,3,4,5] */
  s2->Belt.Set.Int.toArray /* [2,4,5] */
  s2 == s3 /* true */
  ```
*)

val removeMany: t -> value array -> t
(**
  ```res sig
  let removeMany: (t, array<value>) => t
  ```

  Removes each element of array from set. Unlike [remove](#remove), the reference of return value might be changed even if any values in array not existed in set.

  ```res example
  let set = Belt.Set.Int.fromArray([1, 2, 3, 4])

  let newSet = set->Belt.Set.Int.removeMany([5, 4, 3, 2, 1])
  newSet->Belt.Set.Int.toArray /* [] */
  ```
*)

val union: t -> t -> t
(**
  ```res sig
  let union: (t, t) => t
  ```

  Returns union of two sets.

  ```res example
  let s0 = Belt.Set.Int.fromArray([5, 2, 3, 5, 6])
  let s1 = Belt.Set.Int.fromArray([5, 2, 3, 1, 5, 4])
  let union = Belt.Set.Int.union(s0, s1)
  union->Belt.Set.Int.toArray /* [1,2,3,4,5,6] */
  ```
*)

val intersect: t -> t -> t
(**
  ```res sig
  let intersect: (t, t) => t
  ```

  Returns intersection of two sets.

  ```res example
  let s0 = Belt.Set.Int.fromArray([5, 2, 3, 5, 6])
  let s1 = Belt.Set.Int.fromArray([5, 2, 3, 1, 5, 4])
  let intersect = Belt.Set.Int.intersect(s0, s1)
  intersect->Belt.Set.Int.toArray /* [2,3,5] */
  ```
*)

val diff: t -> t -> t
(**
  ```res sig
  let diff: (t, t) => t
  ```

  Returns elements from first set, not existing in second set.

  ```res example
  let s0 = Belt.Set.Int.fromArray([5, 2, 3, 5, 6])
  let s1 = Belt.Set.Int.fromArray([5, 2, 3, 1, 5, 4])
  Belt.Set.Int.toArray(Belt.Set.Int.diff(s0, s1)) /* [6] */
  Belt.Set.Int.toArray(Belt.Set.Int.diff(s1, s0)) /* [1,4] */
  ```
*)

val subset: t -> t -> bool
(**
  ```res sig
  let subset: (t, t) => bool
  ```

  Checks if second set is subset of first set.

  ```res example
  let s0 = Belt.Set.Int.fromArray([5, 2, 3, 5, 6])
  let s1 = Belt.Set.Int.fromArray([5, 2, 3, 1, 5, 4])
  let s2 = Belt.Set.Int.intersect(s0, s1)
  Belt.Set.Int.subset(s2, s0) /* true */
  Belt.Set.Int.subset(s2, s1) /* true */
  Belt.Set.Int.subset(s1, s0) /* false */
  ```
*)

val cmp: t -> t -> int
(**
  ```res sig
  let cmp: (t, t) => int
  ```

  Total ordering between sets. Can be used as the ordering function for doing sets of sets. It compares size first and then iterates over each element following the order of elements.
*)

val eq: t -> t -> bool
(**
  ```res sig
  let eq: (t, t) => bool
  ```

  Checks if two sets are equal.

  ```res example
  let s0 = Belt.Set.Int.fromArray([5, 2, 3])
  let s1 = Belt.Set.Int.fromArray([3, 2, 5])

  Belt.Set.Int.eq(s0, s1) /* true */
  ```
*)

val forEachU: t -> (value -> unit [@bs]) -> unit
(**
  ```res sig
  let forEachU: (t, (. value) => unit) => unit
  ```

  Same as [forEach](##forEach) but takes uncurried functon.
*)

val forEach: t -> (value -> unit) -> unit
(**
  ```res sig
  let forEach: (t, value => unit) => unit
  ```

  Applies function `f` in turn to all elements of set in increasing order.

  ```res example
  let s0 = Belt.Set.Int.fromArray([5, 2, 3, 5, 6])
  let acc = ref(list{})
  s0->Belt.Set.Int.forEach(x => acc := Belt.List.add(acc.contents, x))
  acc /* [6,5,3,2] */
  ```
*)

val reduceU: t -> 'a -> ('a -> value -> 'a [@bs]) -> 'a
(**
  ```res sig
  let reduceU: (t, 'a, (. 'a, value) => 'a) => 'a
  ```
*)

val reduce: t -> 'a -> ('a -> value -> 'a) -> 'a
(**
  ```res sig
  let reduce: (t, 'a, ('a, value) => 'a) => 'a
  ```

  Applies function `f` to each element of set in increasing order. Function `f` has two parameters: the item from the set and an “accumulator”, which starts with a value of `initialValue`. `reduce` returns the final value of the accumulator.

  ```res example
  let s0 = Belt.Set.Int.fromArray([5, 2, 3, 5, 6])
  s0->Belt.Set.Int.reduce(list{}, (acc, element) => acc->Belt.List.add(element)) /* [6,5,3,2] */
  ```
*)

val everyU: t -> (value -> bool [@bs]) -> bool
(**
  ```res sig
  let everyU: (t, (. value) => bool) => bool
  ```
*)

val every: t -> (value -> bool) -> bool
(**
  ```res sig
  let every: (t, value => bool) => bool
  ```

  Checks if all elements of the set satisfy the predicate. Order unspecified.

  ```res example
  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.Set.Int.fromArray([2, 4, 6, 8])
  s0->Belt.Set.Int.every(isEven) /* true */
  ```
*)

val someU: t -> (value -> bool [@bs]) -> bool
(**
  ```res sig
  let someU: (t, (. value) => bool) => bool
  ```
*)

val some: t -> (value -> bool) -> bool
(**
  ```res sig
  let some: (t, value => bool) => bool
  ```

  Checks if at least one element of the set satisfies the predicate.

  ```res example
  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.Set.Int.fromArray([1, 2, 4, 6, 8])
  s0->Belt.Set.Int.some(isOdd) /* true */
  ```
*)

val keepU: t -> (value -> bool [@bs]) -> t
(**
  ```res sig
  let keepU: (t, (. value) => bool) => t
  ```
*)

val keep: t -> (value -> bool) -> t
(**
  ```res sig
  let keep: (t, value => bool) => t
  ```

  Returns the set of all elements that satisfy the predicate.

  ```res example
  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.Set.Int.fromArray([1, 2, 3, 4, 5])
  let s1 = s0->Belt.Set.Int.keep(isEven)

  s1->Belt.Set.Int.toArray /* [2,4] */
  ```
*)

val partitionU: t -> (value -> bool [@bs]) -> t * t
(**
  ```res sig
  let partitionU: (t, (. value) => bool) => (t, t)
  ```
*)

val partition: t -> (value -> bool) -> t * t
(**
  ```res sig
  let partition: (t, value => bool) => (t, t)
  ```

  Returns a pair of sets, where first is the set of all the elements of set that satisfy the predicate, and second is the set of all the elements of set that do not satisfy the predicate.

  ```res example
  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.Set.Int.fromArray([1, 2, 3, 4, 5])
  let (s1, s2) = s0->Belt.Set.Int.partition(isOdd)

  s1->Belt.Set.Int.toArray /* [1,3,5] */
  s2->Belt.Set.Int.toArray /* [2,4] */
  ```
*)

val size: t -> int
(**
  ```res sig
  let size: t => int
  ```

  Returns size of the set.

  ```res example
  let s0 = Belt.Set.Int.fromArray([1, 2, 3, 4])

  s0->Belt.Set.Int.size /* 4 */
  ```
*)

val toList: t -> value list
(**
  ```res sig
  let toList: t => list<value>
  ```

  Returns list of ordered set elements.

  ```res example
  let s0 = Belt.Set.Int.fromArray([3, 2, 1, 5])

  s0->Belt.Set.Int.toList /* [1,2,3,5] */
  ```
*)

val toArray: t -> value array
(**
  ```res sig
  let toArray: t => array<value>
  ```

  Returns array of ordered set elements.

  ```res example
  let s0 = Belt.Set.Int.fromArray([3, 2, 1, 5])

  s0->Belt.Set.Int.toArray /* [1,2,3,5] */
  ```
*)

val minimum: t -> value option
(**
  ```res sig
  let minimum: t => option<value>
  ```

  Returns minimum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.Set.Int.empty
  let s1 = Belt.Set.Int.fromArray([3, 2, 1, 5])

  s0->Belt.Set.Int.minimum /* None */
  s1->Belt.Set.Int.minimum /* Some(1) */
  ```
*)

val minUndefined: t -> value Js.undefined
(**
  ```res sig
  let minUndefined: t => Js.undefined<value>
  ```

  Returns minimum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.Set.Int.empty
  let s1 = Belt.Set.Int.fromArray([3, 2, 1, 5])

  s0->Belt.Set.Int.minUndefined /* undefined */
  s1->Belt.Set.Int.minUndefined /* 1 */
  ```
*)

val maximum: t -> value option
(**
  ```res sig
  let maximum: t => option<value>
  ```

  Returns maximum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.Set.Int.empty
  let s1 = Belt.Set.Int.fromArray([3, 2, 1, 5])

  s0->Belt.Set.Int.maximum /* None */
  s1->Belt.Set.Int.maximum /* Some(5) */
  ```
*)

val maxUndefined: t -> value Js.undefined
(**
  ```res sig
  let maxUndefined: t => Js.undefined<value>
  ```

  Returns maximum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.Set.Int.empty
  let s1 = Belt.Set.Int.fromArray([3, 2, 1, 5])

  s0->Belt.Set.Int.maxUndefined /* undefined */
  s1->Belt.Set.Int.maxUndefined /* 5 */
  ```
*)

val get: t -> value -> value option
(**
  ```res sig
  let get: (t, value) => option<value>
  ```

  Returns the reference of the value which is equivalent to value using the comparator specifiecd by this collection. Returns `None` if element does not exist.

  ```res example
  let s0 = Belt.Set.Int.fromArray([1, 2, 3, 4, 5])

  s0->Belt.Set.Int.get(3) /* Some(3) */
  s0->Belt.Set.Int.get(20) /* None */
  ```
*)

val getUndefined: t -> value -> value Js.undefined
(**
  ```res sig
  let getUndefined: (t, value) => Js.undefined<value>
  ```

  Same as [get](#get) but returns `undefined` when element does not exist.
*)

val getExn: t -> value -> value
(**
  ```res sig
  let getExn: (t, value) => value
  ```

  Same as [get](#get) but raise when element does not exist.
*)

val split: t -> value -> (t * t) * bool
(**
  ```res sig
  let split: (t, value) => ((t, t), bool)
  ```

  Returns a tuple `((l, r), present)`, where `l` is the set of elements of set that are strictly less than value, `r` is the set of elements of set that are strictly greater than value, `present` is `false` if set contains no element equal to value, or `true` if set contains an element equal to value.

  ```res example
  let s0 = Belt.Set.Int.fromArray([1, 2, 3, 4, 5])

  let ((smaller, larger), present) = s0->Belt.Set.Int.split(3)

  present /* true */
  smaller->Belt.Set.Int.toArray /* [1,2] */
  larger->Belt.Set.Int.toArray /* [4,5] */
  ```
*)

val checkInvariantInternal: t -> unit
(**
  ```res sig
  let checkInvariantInternal: t => unit
  ```

  **raise** when invariant is not held
*)
