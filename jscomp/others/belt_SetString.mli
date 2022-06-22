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

  Specalized when value type is `string`, more efficient than the generic type, its compare behavior is fixed using the built-in comparison.

  **See** [`Belt.Set`]()
*)

# 34 "others/belt_Set.cppo.mli"
type value = string

# 40 "others/belt_Set.cppo.mli"
(** The type of the set elements. *)


type t
(**
  The type of sets.
*)

val empty: t
(**
  Empty set

  ```res example
  let s0 = Belt.Set.String.empty
  ```
*)

val fromArray: value array -> t
(**
  Creates new set from array of elements.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple", "orange", "banana"])

  s0->Belt.Set.String.toArray /* ["apple", "banana", "orange"] */
  ```
*)

val fromSortedArrayUnsafe: value array -> t
(**
  The same as [fromArray][#fromarray] except it is after assuming the input array is already sorted.
*)

val isEmpty: t -> bool
(**
  Checks if set is empty.

  ```res example
  let empty = Belt.Set.String.fromArray([])
  let notEmpty = Belt.Set.String.fromArray(["apple"])

  Belt.Set.String.isEmpty(empty) /* true */
  Belt.Set.String.isEmpty(notEmpty) /* false */
  ```
*)

val has: t -> value -> bool
(**
  Checks if element exists in set.

  ```res example
  let set = Belt.Set.String.fromArray(["apple", "orange", "banana"])

  set->Belt.Set.String.has("strawberry") /* false */
  set->Belt.Set.String.has("apple") /* true */
  ```
*)

val add: t -> value -> t
(**
  Adds element to set. If element existed in set, value is unchanged.

  ```res example
  let s0 = Belt.Set.String.empty
  let s1 = s0->Belt.Set.String.add("apple")
  let s2 = s1->Belt.Set.String.add("banana")
  let s3 = s2->Belt.Set.String.add("banana")
  s0->Belt.Set.String.toArray /* [] */
  s1->Belt.Set.String.toArray /* ["apple"] */
  s2->Belt.Set.String.toArray /* ["apple", "banana"] */
  s3->Belt.Set.String.toArray /* ["apple", "banana"] */
  s2 == s3 /* true */
  ```
*)

val mergeMany: t -> value array -> t
(**
  Adds each element of array to set. Unlike [add](#add), the reference of return value might be changed even if all values in array already exist in set

  ```res example
  let set = Belt.Set.String.empty

  let newSet = set->Belt.Set.String.mergeMany(["apple", "banana", "orange", "strawberry"])

  newSet->Belt.Set.String.toArray /* ["apple", "banana", "orange", "strawberry"] */
  ```
*)

val remove: t -> value -> t
(**
  Removes element from set. If element did not exist in set, value is unchanged.

  ```res example
  let s0 = Belt.Set.String.fromArray(["orange", "banana", "apple"])
  let s1 = s0->Belt.Set.String.remove("apple")
  let s2 = s1->Belt.Set.String.remove("banana")
  let s3 = s2->Belt.Set.String.remove("banana")

  s1->Belt.Set.String.toArray /* ["orange", "banana"] */
  s2->Belt.Set.String.toArray /* ["orange"] */
  s2 == s3 /* true */
  ```
*)

val removeMany: t -> value array -> t
(**
  Removes each element of array from set. Unlike [remove](#remove), the reference of return value might be changed even if any values in array not existed in set.

  ```res example
  let set = Belt.Set.String.fromArray(["apple", "banana", "orange"])

  let newSet = set->Belt.Set.String.removeMany(["strawberry", "apple", "banana", "orange"])
  newSet->Belt.Set.String.toArray /* [] */
  ```
*)

val union: t -> t -> t
(**
  Returns union of two sets.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple", "banana", "orange", "carrot"])
  let s1 = Belt.Set.String.fromArray(["apple", "banana", "orange", "strawberry"])
  let union = Belt.Set.String.union(s0, s1)
  union->Belt.Set.String.toArray /* ["apple", "banana", "carrot", "orange", "strawberry"] */
  ```
*)

val intersect: t -> t -> t
(**
  Returns intersection of two sets.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple", "banana", "orange", "carrot"])
  let s1 = Belt.Set.String.fromArray(["apple", "banana", "orange", "strawberry"])
  let intersect = Belt.Set.String.intersect(s0, s1)
  intersect->Belt.Set.String.toArray /* ["apple", "banana", "orange"] */
  ```
*)

val diff: t -> t -> t
(**
  Returns elements from first set, not existing in second set.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple", "banana", "orange", "carrot"])
  let s1 = Belt.Set.String.fromArray(["apple", "banana", "orange", "strawberry"])
  Belt.Set.String.toArray(Belt.Set.String.diff(s0, s1)) /* ["carrot"] */
  Belt.Set.String.toArray(Belt.Set.String.diff(s1, s0)) /* ["strawberry"] */
  ```
*)

val subset: t -> t -> bool
(**
  Checks if second set is subset of first set.

  ```res example
  let s0 = Belt.Set.String.fromArray(["5", "2", "3", "5", "6"])
  let s1 = Belt.Set.String.fromArray(["5", "2", "3", "1", "5", "4"])
  let s2 = Belt.Set.String.intersect(s0, s1)
  Belt.Set.String.subset(s2, s0) /* true */
  Belt.Set.String.subset(s2, s1) /* true */
  Belt.Set.String.subset(s1, s0) /* false */
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
  let s0 = Belt.Set.String.fromArray(["apple", "orange"])
  let s1 = Belt.Set.String.fromArray(["orange", "apple"])

  Belt.Set.String.eq(s0, s1) /* true */
  ```
*)

val forEachU: t -> (value -> unit [@bs]) -> unit
(**
  Same as [forEach](##forEach) but takes uncurried functon.
*)

val forEach: t -> (value -> unit) -> unit
(**
  Applies function `f` in turn to all elements of set in increasing order.

  ```res example
  let s0 = Belt.Set.String.fromArray(["banana", "orange", "apple"])
  let acc = ref(list{})
  s0->Belt.Set.String.forEach(x => acc := Belt.List.add(acc.contents, x))
  acc /* ["orange", "banana", "apple"] */
  ```
*)

val reduceU: t -> 'a -> ('a -> value -> 'a [@bs]) -> 'a

val reduce: t -> 'a -> ('a -> value -> 'a) -> 'a
(**
  Applies function `f` to each element of set in increasing order. Function `f` has two parameters: the item from the set and an “accumulator”, which starts with a value of `initialValue`. `reduce` returns the final value of the accumulator.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple", "orange"])
  s0->Belt.Set.String.reduce(0, (acc, element) => acc + String.length(element)) /* 11 */
  ```
*)

val everyU: t -> (value -> bool [@bs]) -> bool

val every: t -> (value -> bool) -> bool
(**
  Checks if all elements of the set satisfy the predicate. Order unspecified.

  ```res example
  let hasAtLeastFiveChars = x => String.length(x) >= 5

  let s0 = Belt.Set.String.fromArray(["apple", "carrot"])
  s0->Belt.Set.String.every(hasAtLeastFiveChars) /* true */
  ```
*)

val someU: t -> (value -> bool [@bs]) -> bool

val some: t -> (value -> bool) -> bool
(**
  Checks if at least one element of the set satisfies the predicate.

  ```res example
  let hasFiveChars = x => String.length(x) == 5

  let s0 = Belt.Set.String.fromArray(["strawberry", "apple"])
  s0->Belt.Set.String.some(hasFiveChars) /* true */
  ```
*)

val keepU: t -> (value -> bool [@bs]) -> t

val keep: t -> (value -> bool) -> t
(**
  Returns the set of all elements that satisfy the predicate.

  ```res example
  let hasFiveChars = x => String.length(x) == 5

  let s0 = Belt.Set.String.fromArray(["apple", "orange", "banana"])
  let s1 = s0->Belt.Set.String.keep(hasFiveChars)

  s1->Belt.Set.String.toArray /* ["apple"] */
  ```
*)

val partitionU: t -> (value -> bool [@bs]) -> t * t

val partition: t -> (value -> bool) -> t * t
(**
  Returns a pair of sets, where first is the set of all the elements of set that satisfy the predicate, and second is the set of all the elements of set that do not satisfy the predicate.

  ```res example
  let hasFiveChars = x => String.length(x) == 5

  let s0 = Belt.Set.String.fromArray(["apple", "carrot"])
  let (s1, s2) = s0->Belt.Set.String.partition(hasFiveChars)

  s1->Belt.Set.String.toArray /* ["apple"] */
  s2->Belt.Set.String.toArray /* ["carrot"] */
  ```
*)

val size: t -> int
(**
  Returns size of the set.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple"])

  s0->Belt.Set.String.size /* 1 */
  ```
*)

val toList: t -> value list
(**
  Returns list of ordered set elements.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple", "watermelon"])

  s0->Belt.Set.String.toList /* ["apple", "watermelon"] */
  ```
*)

val toArray: t -> value array
(**
  Returns array of ordered set elements.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple", "watermelon"])

  s0->Belt.Set.String.toArray /* ["apple", "watermelon"] */
  ```
*)

val minimum: t -> value option
(**
  Returns minimum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.Set.String.empty
  let s1 = Belt.Set.String.fromArray(["apple", "orange"])

  s0->Belt.Set.String.minimum /* None */
  s1->Belt.Set.String.minimum /* Some("apple") */
  ```
*)

val minUndefined: t -> value Js.undefined
(**
  Returns minimum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.Set.String.empty
  let s1 = Belt.Set.String.fromArray(["apple", "orange"])

  s0->Belt.Set.String.minUndefined /* undefined */
  s1->Belt.Set.String.minUndefined /* "apple" */
  ```
*)

val maximum: t -> value option
(**
  Returns maximum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.Set.String.empty
  let s1 = Belt.Set.String.fromArray(["apple", "orange"])

  s0->Belt.Set.String.maximum /* None */
  s1->Belt.Set.String.maximum /* Some("orange") */
  ```
*)

val maxUndefined: t -> value Js.undefined
(**
  Returns maximum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.Set.String.empty
  let s1 = Belt.Set.String.fromArray(["apple", "orange"])

  s0->Belt.Set.String.maxUndefined /* undefined */
  s1->Belt.Set.String.maxUndefined /* orange */
  ```
*)

val get: t -> value -> value option
(**
  Returns the reference of the value which is equivalent to value using the comparator specifiecd by this collection. Returns `None` if element does not exist.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple", "carrot"])

  s0->Belt.Set.String.get("carrot") /* Some("carrot") */
  s0->Belt.Set.String.get("watermelon") /* None */
  ```
*)

val getUndefined: t -> value -> value Js.undefined
(**
  See [get](#get) - returns `undefined` when element does not exist.
*)

val getExn: t -> value -> value
(**
  See [get](#get) - raise when element does not exist.
*)

val split: t -> value -> (t * t) * bool
(**
  Returns a triple `((l, r), present)`, where `l` is the set of elements of set that are strictly less than value, `r` is the set of elements of set that are strictly greater than value, `present` is `false` if set contains no element equal to value, or `true` if set contains an element equal to value.

  ```res example
  let s0 = Belt.Set.String.fromArray(["apple", "banana", "orange"])

  let ((smaller, larger), present) = s0->Belt.Set.String.split("banana")

  present /* true */
  smaller->Belt.Set.String.toArray /* ["apple"] */
  larger->Belt.Set.String.toArray /* ["orange"] */
  ```
*)

val checkInvariantInternal: t -> unit
(**
  **raise** when invariant is not held
*)
