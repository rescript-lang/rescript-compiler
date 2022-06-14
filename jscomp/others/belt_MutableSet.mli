
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
  A **mutable** sorted set module which allows customized compare behavior.
  The implementation uses balanced binary trees, and therefore searching and insertion take time logarithmic in the size of the map.

  It also has two specialized inner modules [Belt.MutableSet.Int](mutable-set-int) and [Belt.MutableSet.String](mutable-set-string) - This module separates data from function which is more verbose but slightly more efficient

  ```res example
  module PairComparator = Belt.Id.MakeComparable({
    type t = (int, int)
    let cmp = ((a0, a1), (b0, b1)) =>
      switch Pervasives.compare(a0, b0) {
      | 0 => Pervasives.compare(a1, b1)
      | c => c
      }
  })

  let mySet = Belt.MutableSet.make(~id=module(PairComparator))
  mySet->Belt.MutableSet.add((1, 2))
  ```
*)


(** Specialized when key type is `int`, more efficient
    than the generic type
*)
module Int = Belt_MutableSetInt

(** Specialized when key type is `string`, more efficient
    than the generic type *)
module String = Belt_MutableSetString

type ('value, 'identity) t
(**
  `'value` is the element type

  `'identity` the identity of the collection
*)

type ('value, 'id) id = ('value, 'id) Belt_Id.comparable
(**
  The identity needed for making a set from scratch
*)

val make: id:('value, 'id) id -> ('value, 'id) t
(**
  Creates a new set by taking in the comparator
*)

val fromArray: 'value array -> id:('value, 'id) id ->   ('value, 'id) t
(**
  Creates new set from array of elements.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

  s0->Belt.MutableSet.toArray /* [1, 2, 3, 4] */
  ```
*)

val fromSortedArrayUnsafe: 'value array -> id:('value, 'id) id ->  ('value,'id) t
(**
  The same as [fromArray][#fromarray] except it is after assuming the input array is already sorted.
*)

val copy: ('value, 'id) t -> ('value, 'id) t
(**
  Returns copy of a set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

  let copied = s0->Belt.MutableSet.copy
  copied->Belt.MutableSet.toArray /* [1, 2, 3, 4] */
  ```
*)

val isEmpty: _ t -> bool
(**
  Checks if set is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let empty = Belt.MutableSet.fromArray([], ~id=module(IntCmp))
  let notEmpty = Belt.MutableSet.fromArray([1], ~id=module(IntCmp))

  Belt.MutableSet.isEmpty(empty) /* true */
  Belt.MutableSet.isEmpty(notEmpty) /* false */
  ```
*)

val has:  ('value, 'id) t -> 'value ->  bool
(**
  Checks if element exists in set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let set = Belt.MutableSet.fromArray([1, 4, 2, 5], ~id=module(IntCmp))

  set->Belt.MutableSet.has(3) /* false */
  set->Belt.MutableSet.has(1) /* true */
  ```
*)

val add: ('value, 'id) t -> 'value -> unit
(**
  Adds element to set. If element existed in set, value is unchanged.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.make(~id=module(IntCmp))
  s0->Belt.MutableSet.add(1)
  s0->Belt.MutableSet.add(2)
  s0->Belt.MutableSet.add(2)

  s0->Belt.MutableSet.toArray /* [1, 2] */
  ```
*)

val addCheck:
  ('value, 'id) t -> 'value -> bool

val mergeMany:
  ('value, 'id) t -> 'value array -> unit
(**
  Adds each element of array to set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let set = Belt.MutableSet.make(~id=module(IntCmp))

  set->Belt.MutableSet.mergeMany([5, 4, 3, 2, 1])
  set->Belt.MutableSet.toArray /* [1, 2, 3, 4, 5] */
  ```
*)

val remove: ('value, 'id) t -> 'value -> unit
(**
  Removes element from set. If element did not exist in set, value is unchanged.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([2, 3, 1, 4, 5], ~id=module(IntCmp))
  s0->Belt.MutableSet.remove(1)
  s0->Belt.MutableSet.remove(3)
  s0->Belt.MutableSet.remove(3)

  s0->Belt.MutableSet.toArray /* [2,4,5] */
  ```
*)

val removeCheck: ('value, 'id) t -> 'value -> bool
(* `b = removeCheck s e` `b` is true means one element removed *)

val removeMany:
  ('value, 'id) t -> 'value array -> unit
(**
  Removes each element of array from set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let set = Belt.MutableSet.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

  set->Belt.MutableSet.removeMany([5, 4, 3, 2, 1])
  set->Belt.MutableSet.toArray /* [] */
  ```
*)

val union: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
(**
  Returns union of two sets.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
  let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
  let union = Belt.MutableSet.union(s0, s1)
  union->Belt.MutableSet.toArray /* [1,2,3,4,5,6] */
  ```
*)

val intersect: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
(**
  Returns intersection of two sets.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
  let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
  let intersect = Belt.MutableSet.intersect(s0, s1)
  intersect->Belt.MutableSet.toArray /* [2,3,5] */
  ```
*)

val diff: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
(**
  Returns elements from first set, not existing in second set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
  let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
  Belt.MutableSet.toArray(Belt.MutableSet.diff(s0, s1)) /* [6] */
  Belt.MutableSet.toArray(Belt.MutableSet.diff(s1, s0)) /* [1,4] */
  ```
*)

val subset: ('value, 'id) t -> ('value, 'id) t -> bool
(**
  Checks if second set is subset of first set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
  let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
  let s2 = Belt.MutableSet.intersect(s0, s1)
  Belt.MutableSet.subset(s2, s0) /* true */
  Belt.MutableSet.subset(s2, s1) /* true */
  Belt.MutableSet.subset(s1, s0) /* false */
  ```
*)

val cmp:
  ('value, 'id) t -> ('value, 'id) t -> int
(**
  Total ordering between sets. Can be used as the ordering function for doing sets of sets. It compares size first and then iterates over each element following the order of elements.
*)

val eq:
  ('value, 'id) t -> ('value, 'id) t -> bool
(**
  Checks if two sets are equal.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([5, 2, 3], ~id=module(IntCmp))
  let s1 = Belt.MutableSet.fromArray([3, 2, 5], ~id=module(IntCmp))

  Belt.MutableSet.eq(s0, s1) /* true */
  ```
*)

val forEachU: ('value, 'id) t -> ('value -> unit [@bs]) ->  unit
(**
  Same as [forEach](##forEach) but takes uncurried functon.
*)

val forEach: ('value, 'id) t -> ('value -> unit) ->  unit
(**
  Applies function `f` in turn to all elements of set in increasing order.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
  let acc = ref(list{})
  s0->Belt.MutableSet.forEach(x => acc := Belt.List.add(acc.contents, x))
  acc /* [6,5,3,2] */
  ```
*)

val reduceU: ('value, 'id) t -> 'a -> ('a -> 'value -> 'a [@bs]) -> 'a

val reduce: ('value, 'id) t -> 'a -> ('a -> 'value -> 'a) -> 'a
(**
  Applies function `f` to each element of set in increasing order. Function `f` has two parameters: the item from the set and an “accumulator”, which starts with a value of `initialValue`. `reduce` returns the final value of the accumulator.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
  s0->Belt.MutableSet.reduce(list{}, (acc, element) => acc->Belt.List.add(element)) /* [6,5,3,2] */
  ```
*)

val everyU: ('value, 'id) t -> ('value -> bool [@bs]) -> bool

val every: ('value, 'id) t -> ('value -> bool) -> bool
(**
  Checks if all elements of the set satisfy the predicate. Order unspecified.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.MutableSet.fromArray([2, 4, 6, 8], ~id=module(IntCmp))
  s0->Belt.MutableSet.every(isEven) /* true */
  ```
*)

val someU: ('value, 'id) t ->  ('value -> bool [@bs]) -> bool

val some: ('value, 'id) t ->  ('value -> bool) -> bool
(**
  Checks if at least one element of the set satisfies the predicate.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.MutableSet.fromArray([1, 2, 4, 6, 8], ~id=module(IntCmp))
  s0->Belt.MutableSet.some(isOdd) /* true */
  ```
*)

val keepU: ('value, 'id) t -> ('value -> bool [@bs]) -> ('value, 'id) t

val keep: ('value, 'id) t -> ('value -> bool) -> ('value, 'id) t
(**
  Returns the set of all elements that satisfy the predicate.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
  let s1 = s0->Belt.MutableSet.keep(isEven)

  s1->Belt.MutableSet.toArray /* [2, 4] */
  ```
*)

val partitionU: ('value, 'id) t -> ('value -> bool [@bs]) -> ('value, 'id) t * ('value, 'id) t

val partition: ('value, 'id) t -> ('value -> bool) -> ('value, 'id) t * ('value, 'id) t
(**
  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
  let (s1, s2) = s0->Belt.MutableSet.partition(isOdd)

  s1->Belt.MutableSet.toArray /* [1,3,5] */
  s2->Belt.MutableSet.toArray /* [2,4] */
  ```
*)

val size:  ('value, 'id) t -> int
(**
  Returns size of the set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

  s0->Belt.MutableSet.size /* 4 */
  ```
*)

val toList: ('value, 'id) t -> 'value list
(**
  Returns list of ordered set elements.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

  s0->Belt.MutableSet.toList /* [1,2,3,5] */
  ```
*)

val toArray: ('value, 'id) t -> 'value array
(**
  Returns array of ordered set elements.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

  s0->Belt.MutableSet.toArray /* [1,2,3,5] */
  ```
*)

val minimum: ('value, 'id) t -> 'value option
(**
  Returns minimum value of the collection. `None` if collection is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.make(~id=module(IntCmp))
  let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

  s0->Belt.MutableSet.minimum /* None */
  s1->Belt.MutableSet.minimum /* Some(1) */
  ```
*)

val minUndefined: ('value, 'id) t -> 'value Js.undefined
(**
  Returns minimum value of the collection. `undefined` if collection is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.make(~id=module(IntCmp))
  let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

  s0->Belt.MutableSet.minUndefined /* undefined */
  s1->Belt.MutableSet.minUndefined /* 1 */
  ```
*)

val maximum: ('value, 'id) t -> 'value option
(**
  Returns maximum value of the collection. `None` if collection is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.make(~id=module(IntCmp))
  let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

  s0->Belt.MutableSet.maximum /* None */
  s1->Belt.MutableSet.maximum /* Some(5) */
  ```
*)

val maxUndefined: ('value, 'id) t -> 'value Js.undefined
(**
  Returns maximum value of the collection. `undefined` if collection is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.make(~id=module(IntCmp))
  let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

  s0->Belt.MutableSet.maxUndefined /* undefined */
  s1->Belt.MutableSet.maxUndefined /* 5 */
  ```
*)

val get: ('value, 'id) t -> 'value -> 'value option
(**
  Returns the reference of the value which is equivalent to value using the comparator specifiecd by this collection. Returns `None` if element does not exist.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

  s0->Belt.MutableSet.get(3) /* Some(3) */
  s0->Belt.MutableSet.get(20) /* None */
  ```
*)

val getUndefined: ('value, 'id) t -> 'value -> 'value Js.undefined
(**
  Same as [get](#get) but returns `undefined` when element does not exist.
*)

val getExn: ('value, 'id) t -> 'value -> 'value
(**
  Same as [get](#get) but raise when element does not exist.
*)

val split: ('value, 'id) t -> 'value ->  (('value, 'id) t * ('value, 'id) t) * bool
(**
  Returns a tuple `((smaller, larger), present)`, `present` is true when element exist in set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

  let ((smaller, larger), present) = s0->Belt.MutableSet.split(3)

  present /* true */
  smaller->Belt.MutableSet.toArray /* [1,2] */
  larger->Belt.MutableSet.toArray /* [4,5] */
  ```
*)

val checkInvariantInternal: _ t -> unit
(**
  **raise** when invariant is not held
*)

(*
  `add0` was not exposed for various reasons:
  1. such api is dangerious
  [ cmp: ('value,'id) Belt_Cmp.cmp ->
    ('value, 'id) t0 -> 'value ->
    ('value, 'id) t0]
  2. It is not really significantly more *)
