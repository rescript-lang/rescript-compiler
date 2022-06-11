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

  This module is [Belt.MutableSet](mutable-set) specialized with key type to be a `string` type.
  It is more efficient in general, the API is the same with [Belt.MutableSet](mutable-set) except its key type is fixed, and identity is not needed (using the built-in one)

  **See** [`Belt.MutableSet`]()
*)


# 36 "others/setm.cppo.mli"
type value = string
(**
  ```res prelude
  type value = string
  ```

  The type of the set elements.
*)

# 42 "others/setm.cppo.mli"
  (** The type of the set elements. *)


type t
(**
  ```res prelude
  type t
  ```

  The type of sets.
*)

val make: unit -> t
(**
  ```res sig
  let make: unit => t
  ```

  Returns empty set.

  ```res example
  let set = Belt.MutableSet.String.make()
  ```
*)

val fromArray: value array -> t
(**
  ```res sig
  let fromArray: array<value> => t
  ```

  Creates new set from array of elements.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple", "orange", "banana"])

  s0->Belt.MutableSet.String.toArray /* ["apple", "banana", "orange"] */
  ```
*)

val fromSortedArrayUnsafe: value array -> t
(**
  ```res sig
  let fromSortedArrayUnsafe: array<value> => t
  ```

  The same as [fromArray][#fromarray] except it is after assuming the input array is already sorted.
*)

val copy: t -> t
(**
  ```res sig
  let copy: t => t
  ```

  Returns copy of a set.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["orange", "apple"])

  let copied = s0->Belt.MutableSet.String.copy
  copied->Belt.MutableSet.String.toArray /* ["apple", "orange"] */
  ```
*)

val isEmpty: t -> bool
(**
  ```res sig
  let isEmpty: t => bool
  ```

  Checks if set is empty.

  ```res example
  let empty = Belt.MutableSet.String.fromArray([])
  let notEmpty = Belt.MutableSet.String.fromArray(["apple"])

  Belt.MutableSet.String.isEmpty(empty) /* true */
  Belt.MutableSet.String.isEmpty(notEmpty) /* false */
  ```
*)

val has: t -> value -> bool
(**
  ```res sig
  let has: (t, value) => bool
  ```

  Checks if element exists in set.

  ```res example
  let set = Belt.MutableSet.String.fromArray(["apple", "orange", "banana"])

  set->Belt.MutableSet.String.has("strawberry") /* false */
  set->Belt.MutableSet.String.has("apple") /* true */
  ```
*)

val add: t -> value -> unit
(**
  ```res sig
  let add: (t, value) => unit
  ```

  Adds element to set. If element existed in set, value is unchanged.

  ```res example
  let s0 = Belt.MutableSet.String.make()
  s0->Belt.MutableSet.String.add("apple")
  s0->Belt.MutableSet.String.add("banana")
  s0->Belt.MutableSet.String.add("banana")

  s0->Belt.MutableSet.String.toArray /* ["apple", "banana"] */
  ```
*)

val addCheck: t -> value -> bool
(**
  ```res sig
  let addCheck: (t, value) => bool
  ```
*)

val mergeMany: t -> value array -> unit
(**
  ```res sig
  let mergeMany: (t, array<value>) => unit
  ```

  Adds each element of array to set.

  ```res example
  let set = Belt.MutableSet.String.make()

  set->Belt.MutableSet.String.mergeMany(["apple", "banana", "orange", "strawberry"])
  set->Belt.MutableSet.String.toArray /* ["apple", "banana", "orange", "strawberry"] */
  ```
*)

val remove: t -> value -> unit
(**
  ```res sig
  let remove: (t, value) => unit
  ```

  Removes element from set. If element wasn't existed in set, value is unchanged.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["orange", "banana", "apple"])
  s0->Belt.MutableSet.String.remove("apple")
  s0->Belt.MutableSet.String.remove("banana")
  s0->Belt.MutableSet.String.remove("banana")

  s0->Belt.MutableSet.String.toArray /* ["orange"] */
  ```
*)

val removeCheck: t -> value -> bool
(**
  ```res sig
  let removeCheck: (t, value) => bool
  ```
*)

val removeMany: t -> value array -> unit
(**
  ```res sig
  let removeMany: (t, array<value>) => unit
  ```

  Removes each element of array from set.

  ```res example
  let set = Belt.MutableSet.String.fromArray(["apple", "banana", "orange"])

  set->Belt.MutableSet.String.removeMany(["strawberry", "apple", "banana", "orange"])
  set->Belt.MutableSet.String.toArray /* [] */
  ```
*)

val union: t -> t -> t
(**
  ```res sig
  let union: (t, t) => t
  ```

  Returns union of two sets.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple", "banana", "orange", "carrot"])
  let s1 = Belt.MutableSet.String.fromArray(["apple", "banana", "orange", "strawberry"])
  let union = Belt.MutableSet.String.union(s0, s1)
  union->Belt.MutableSet.String.toArray /* ["apple", "banana", "carrot", "orange", "strawberry"] */
  ```
*)

val intersect: t -> t -> t
(**
  ```res sig
  let intersect: (t, t) => t
  ```

  Returns intersection of two sets.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple", "banana", "orange", "carrot"])
  let s1 = Belt.MutableSet.String.fromArray(["apple", "banana", "orange", "strawberry"])
  let intersect = Belt.MutableSet.String.intersect(s0, s1)
  intersect->Belt.MutableSet.String.toArray /* ["apple", "banana", "orange"] */
  ```
*)

val diff: t -> t -> t
(**
  ```res sig
  let diff: (t, t) => t
  ```

  Returns elements from first set, not existing in second set.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple", "banana", "orange", "carrot"])
  let s1 = Belt.MutableSet.String.fromArray(["apple", "banana", "orange", "strawberry"])
  Belt.MutableSet.String.toArray(Belt.MutableSet.String.diff(s0, s1)) /* ["carrot"] */
  Belt.MutableSet.String.toArray(Belt.MutableSet.String.diff(s1, s0)) /* ["strawberry"] */
  ```
*)

val subset: t -> t -> bool
(**
  ```res sig
  let subset: (t, t) => bool
  ```

  Checks if second set is subset of first set.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["5", "2", "3", "5", "6"])
  let s1 = Belt.MutableSet.String.fromArray(["5", "2", "3", "1", "5", "4"])
  let s2 = Belt.MutableSet.String.intersect(s0, s1)
  Belt.MutableSet.String.subset(s2, s0) /* true */
  Belt.MutableSet.String.subset(s2, s1) /* true */
  Belt.MutableSet.String.subset(s1, s0) /* false */
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
  let s0 = Belt.MutableSet.String.fromArray(["apple", "orange"])
  let s1 = Belt.MutableSet.String.fromArray(["orange", "apple"])

  Belt.MutableSet.String.eq(s0, s1) /* true */
  ```
*)

val forEachU: t -> (value -> unit [@bs]) ->  unit
(**
  ```res sig
  let forEachU: (t, (. value) => unit) => unit
  ```

  Same as [forEach](##forEach) but takes uncurried functon.
*)

val forEach: t -> (value -> unit ) ->  unit
(**
  ```res sig
  let forEach: (t, value => unit) => unit
  ```

  Applies function `f` in turn to all elements of set in increasing order.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["banana", "orange", "apple"])
  let acc = ref(list{})
  s0->Belt.MutableSet.String.forEach(x => acc := Belt.List.add(acc.contents, x))
  acc /* ["orange", "banana", "apple"] */
  ```
*)

val reduceU: t -> 'a -> ('a -> value -> 'a [@bs]) -> 'a
(**
  ```res sig
  let reduceU: (t, 'a, (. 'a, value) => 'a) => 'a
  ```
*)

val reduce: t -> 'a -> ('a -> value -> 'a ) -> 'a
(**
  ```res sig
  let reduce: (t, 'a, ('a, value) => 'a) => 'a
  ```

  Applies function `f` to each element of set in increasing order. Function `f` has two parameters: the item from the set and an “accumulator”, which starts with a value of `initialValue`. `reduce` returns the final value of the accumulator.

  Applies function `f` to each element of set in increasing order. Function `f` has two parameters: the item from the set and an “accumulator”, which starts with a value of `initialValue`. `reduce` returns the final value of the accumulator.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple", "orange"])
  s0->Belt.MutableSet.String.reduce(0, (acc, element) => acc + String.length(element)) /* 11 */
  ```
*)

val everyU: t -> (value -> bool [@bs]) -> bool
(**
  ```res sig
  let everyU: (t, (. value) => bool) => bool
  ```
*)

val every: t -> (value -> bool) ->  bool
(**
  ```res sig
  let every: (t, value => bool) => bool
  ```

  Checks if all elements of the set satisfy the predicate. Order unspecified.

  ```res example
  let hasAtLeastFiveChars = x => String.length(x) >= 5

  let s0 = Belt.MutableSet.String.fromArray(["apple", "carrot"])
  s0->Belt.MutableSet.String.every(hasAtLeastFiveChars) /* true */
  ```
*)

val someU: t -> (value -> bool [@bs]) -> bool
(**
  ```res sig
  let someU: (t, (. value) => bool) => bool
  ```
*)

val some: t -> (value -> bool) ->  bool
(**
  ```res sig
  let some: (t, value => bool) => bool
  ```

  Checks if at least one element of the set satisfies the predicate.

  ```res example
  let hasFiveChars = x => String.length(x) == 5

  let s0 = Belt.MutableSet.String.fromArray(["strawberry", "apple"])
  s0->Belt.MutableSet.String.some(hasFiveChars) /* true */
  ```
*)

val keepU: t -> (value -> bool [@bs]) ->  t
(**
  ```res sig
  let keepU: (t, (. value) => bool) => t
  ```
*)

val keep: t -> (value -> bool) ->  t
(**
  ```res sig
  let keep: (t, value => bool) => t
  ```

  Returns the set of all elements that satisfy the predicate.

  ```res example
  let hasFiveChars = x => String.length(x) == 5

  let s0 = Belt.MutableSet.String.fromArray(["apple", "orange", "banana"])
  let s1 = s0->Belt.MutableSet.String.keep(hasFiveChars)

  s1->Belt.MutableSet.String.toArray /* ["apple"] */
  ```
*)

val partitionU: t -> (value -> bool [@bs]) ->  t * t
(**
  ```res sig
  let partitionU: (t, (. value) => bool) => (t, t)
  ```
*)

val partition: t -> (value -> bool) ->  t * t
(**
  ```res sig
  let partition: (t, value => bool) => (t, t)
  ```

  Returns a pair of sets, where first is the set of all the elements of set that satisfy the predicate, and second is the set of all the elements of set that do not satisfy the predicate.

  ```res example
  let hasFiveChars = x => String.length(x) == 5

  let s0 = Belt.MutableSet.String.fromArray(["apple", "carrot"])
  let (s1, s2) = s0->Belt.MutableSet.String.partition(hasFiveChars)

  s1->Belt.MutableSet.String.toArray /* ["apple"] */
  s2->Belt.MutableSet.String.toArray /* ["carrot"] */
  ```
*)

val size: t -> int
(**
  ```res sig
  let size: t => int
  ```

  Returns size of the set.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple"])

  s0->Belt.MutableSet.String.size /* 1 */
  ```
*)

val toList: t -> value list
(**
  ```res sig
  let toList: t => list<value>
  ```

  Returns list of ordered set elements.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple", "watermelon"])

  s0->Belt.MutableSet.String.toList /* ["apple", "watermelon"] */
  ```
*)

val toArray: t -> value array
(**
  ```res sig
  let toArray: t => array<value>
  ```

  Returns array of ordered set elements.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple", "watermelon"])

  s0->Belt.MutableSet.String.toArray /* ["apple", "watermelon"] */
  ```
*)

val minimum: t -> value option
(**
  ```res sig
  let minimum: t => option<value>
  ```

  Returns minimum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.MutableSet.String.make()
  let s1 = Belt.MutableSet.String.fromArray(["apple", "orange"])

  s0->Belt.MutableSet.String.minimum /* None */
  s1->Belt.MutableSet.String.minimum /* Some("apple") */
  ```
*)

val minUndefined: t -> value Js.undefined
(**
  ```res sig
  let minUndefined: t => Js.undefined<value>
  ```

  Returns minimum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.MutableSet.String.make()
  let s1 = Belt.MutableSet.String.fromArray(["apple", "orange"])

  s0->Belt.MutableSet.String.minUndefined /* undefined */
  s1->Belt.MutableSet.String.minUndefined /* "apple" */
  ```
*)

val maximum: t -> value option
(**
  ```res sig
  let maximum: t => option<value>
  ```

  Returns maximum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.MutableSet.String.make()
  let s1 = Belt.MutableSet.String.fromArray(["apple", "orange"])

  s0->Belt.MutableSet.String.maximum /* None */
  s1->Belt.MutableSet.String.maximum /* Some("orange") */
  ```
*)

val maxUndefined: t -> value Js.undefined
(**
  ```res sig
  let maxUndefined: t => Js.undefined<value>
  ```

  Returns maximum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.MutableSet.String.make()
  let s1 = Belt.MutableSet.String.fromArray(["apple", "orange"])

  s0->Belt.MutableSet.String.maxUndefined /* undefined */
  s1->Belt.MutableSet.String.maxUndefined /* orange */
  ```
*)

val get:  t -> value -> value option
(**
  ```res sig
  let get: (t, value) => option<value>
  ```

  Returns the reference of the value which is equivalent to value using the comparator specifiecd by this collection. Returns `None` if element does not exist.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple", "carrot"])

  s0->Belt.MutableSet.String.get("carrot") /* Some("carrot") */
  s0->Belt.MutableSet.String.get("watermelon") /* None */
  ```
*)

val getUndefined:  t -> value -> value Js.undefined
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

val split:  t -> value  -> (t * t) * bool
(**
  ```res sig
  let split: (t, value) => ((t, t), bool)
  ```

  Returns a tuple `((smaller, larger), present)`, `present` is true when element exist in set.

  ```res example
  let s0 = Belt.MutableSet.String.fromArray(["apple", "banana", "orange"])

  let ((smaller, larger), present) = s0->Belt.MutableSet.String.split("banana")

  present /* true */
  smaller->Belt.MutableSet.String.toArray /* ["apple"] */
  larger->Belt.MutableSet.String.toArray /* ["orange"] */
  ```
*)

val checkInvariantInternal: t -> unit
(**
  ```res sig
  let checkInvariantInternal: t => unit
  ```

  **raise** when invariant is not held
*)
