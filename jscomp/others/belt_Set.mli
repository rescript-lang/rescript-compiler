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

(** An _immutable_ sorted set module which allows customized _compare_ behavior.

  The implementation uses balanced binary trees, and therefore searching
  and insertion take time logarithmic in the size of the map.

  For more info on this module's usage of identity, `make` and others, please see
  the top level documentation of Belt, **A special encoding for collection safety**.

  Example usage:

  ```res example
  module PairComparator =
    Belt.Id.MakeComparable({
      type t = (int, int)
      let cmp = ((a0, a1), (b0, b1)) =>
        switch (Pervasives.compare(a0, b0)) {
        | 0 => Pervasives.compare(a1, b1)
        | c => c
        }
    })

  let mySet = Belt.Set.make(~id=module(PairComparator))
  let mySet2 = Belt.Set.add(mySet, (1, 2))
  ```

  **Note:** This module's examples will assume a predeclared module for integers
  called `IntCmp`. It is declared like this:

  ```res example
  module IntCmp =
    Belt.Id.MakeComparable({
      type t = int
      let cmp = Pervasives.compare
    })
  ```
*)

(** Specalized when value type is `int`, more efficient
  than the generic type, its compare behavior is fixed using the built-in comparison
*)
module Int = Belt_SetInt

(** Specalized when value type is `string`, more efficient
  than the generic type, its compare behavior is fixed using the built-in comparison
*)
module String = Belt_SetString


(** This module seprate identity from data, it is a bit more verbose but slightly
  more efficient due to the fact that there is no need to pack identity and data back
  after each operation
*)
module Dict = Belt_SetDict


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

  ```res example
  let set = Belt.Set.make(~id=module(IntCmp))
  ```
*)



val fromArray:  'value array -> id:('value, 'id) id ->  ('value, 'id) t
(**
  Creates new set from array of elements.

  ```res example
  let s0 = Belt.Set.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

  s0->Belt.Set.toArray /* [1, 2, 3, 4] */
  ```
*)



val fromSortedArrayUnsafe: 'value array -> id:('value, 'id) id -> ('value,'id) t
(**
  The same as [fromArray][#fromarray] except it is after assuming the input array is already sorted.
*)


val isEmpty: _ t -> bool
(**
   Checks if set is empty.

   ```res example
   let empty = Belt.Set.fromArray([], ~id=module(IntCmp))
   let notEmpty = Belt.Set.fromArray([1],~id=module(IntCmp))

   Belt.Set.isEmpty(empty) /* true */
   Belt.Set.isEmpty(notEmpty) /* false */
   ```
*)

val has: ('value, 'id) t -> 'value ->  bool
(**
   Checks if element exists in set.

   ```res example
   let set = Belt.Set.fromArray([1, 4, 2, 5], ~id=module(IntCmp))

   set->Belt.Set.has(3) /* false */
   set->Belt.Set.has(1) /* true */
   ```
*)

val add:
  ('value, 'id) t -> 'value -> ('value, 'id) t
(**
  Adds element to set. If element existed in set, value is unchanged.

  ```res example
  let s0 = Belt.Set.make(~id=module(IntCmp))
  let s1 = s0->Belt.Set.add(1)
  let s2 = s1->Belt.Set.add(2)
  let s3 = s2->Belt.Set.add(2)
  s0->Belt.Set.toArray /* [] */
  s1->Belt.Set.toArray /* [1] */
  s2->Belt.Set.toArray /* [1, 2] */
  s3->Belt.Set.toArray /* [1,2 ] */
  s2 == s3 /* true */
  ```
*)

val mergeMany: ('value, 'id) t -> 'value array -> ('value, 'id) t
(**
  Adds each element of array to set. Unlike [add](#add), the reference of return value might be changed even if all values in array already exist in set

  ```res example
  let set = Belt.Set.make(~id=module(IntCmp))

  let newSet = set->Belt.Set.mergeMany([5, 4, 3, 2, 1])
  newSet->Belt.Set.toArray /* [1, 2, 3, 4, 5] */
  ```
*)

val remove: ('value, 'id) t -> 'value -> ('value, 'id) t
(**
  Removes element from set. If element wasn't existed in set, value is unchanged.

  ```res example
  let s0 = Belt.Set.fromArray([2,3,1,4,5], ~id=module(IntCmp))
  let s1 = s0->Belt.Set.remove(1)
  let s2 = s1->Belt.Set.remove(3)
  let s3 = s2->Belt.Set.remove(3)

  s1->Belt.Set.toArray /* [2,3,4,5] */
  s2->Belt.Set.toArray /* [2,4,5] */
  s2 == s3 /* true */
  ```
*)

val removeMany:
  ('value, 'id) t -> 'value array -> ('value, 'id) t
(**
  Removes each element of array from set. Unlike [remove](#remove), the reference of return value might be changed even if any values in array not existed in set.

  ```res example
  let set = Belt.Set.fromArray([1, 2, 3, 4],~id=module(IntCmp))

  let newSet = set->Belt.Set.removeMany([5, 4, 3, 2, 1])
  newSet->Belt.Set.toArray /* [] */
  ```
*)

val union: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
(**
   Returns union of two sets.

   ```res example
   let s0 = Belt.Set.fromArray([5,2,3,5,6], ~id=module(IntCmp))
   let s1 = Belt.Set.fromArray([5,2,3,1,5,4], ~id=module(IntCmp))
   let union = Belt.Set.union(s0, s1)
   union->Belt.Set.toArray /* [1,2,3,4,5,6] */
   ```
*)

val intersect: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
(**
  Returns intersection of two sets.

  ```res example
  let s0 = Belt.Set.fromArray([5,2,3,5,6], ~id=module(IntCmp))
  let s1 = Belt.Set.fromArray([5,2,3,1,5,4], ~id=module(IntCmp))
  let intersect = Belt.Set.intersect(s0, s1)
  intersect->Belt.Set.toArray /* [2,3,5] */
  ```
*)

val diff: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
(**
  Returns elements from first set, not existing in second set.

  ```res example
  let s0 = Belt.Set.fromArray([5,2,3,5,6], ~id=module(IntCmp))
  let s1 = Belt.Set.fromArray([5,2,3,1,5,4], ~id=module(IntCmp))
  Belt.Set.toArray(Belt.Set.diff(s0, s1)) /* [6] */
  Belt.Set.toArray(Belt.Set.diff(s1,s0)) /* [1,4] */
  ```
*)

val subset: ('value, 'id) t -> ('value, 'id) t -> bool
(**
  Checks if second set is subset of first set.

  ```res example
  let s0 = Belt.Set.fromArray([5,2,3,5,6], ~id=module(IntCmp))
  let s1 = Belt.Set.fromArray([5,2,3,1,5,4], ~id=module(IntCmp))
  let s2 = Belt.Set.intersect(s0, s1)
  Belt.Set.subset(s2, s0) /* true */
  Belt.Set.subset(s2, s1) /* true */
  Belt.Set.subset(s1, s0) /* false */
  ```
*)

val cmp: ('value, 'id) t -> ('value, 'id) t -> int
(**
  Total ordering between sets. Can be used as the ordering function for doing sets of sets. It compares size first and then iterates over each element following the order of elements.
*)

val eq: ('value, 'id) t -> ('value, 'id) t -> bool
(**
  Checks if two sets are equal.

  ```res example
  let s0 = Belt.Set.fromArray([5,2,3], ~id=module(IntCmp))
  let s1 = Belt.Set.fromArray([3,2,5], ~id=module(IntCmp))

  Belt.Set.eq(s0, s1) /* true */
  ```
*)

val forEachU: ('value, 'id) t -> ('value -> unit [@bs]) ->  unit
(**
  Same as [forEach](##forEach) but takes uncurried functon.
*)

val forEach: ('value, 'id) t -> ('value -> unit ) ->  unit
(**
  Applies function `f` in turn to all elements of set in increasing order.

  ```res example
  let s0 = Belt.Set.fromArray([5,2,3,5,6], ~id=module(IntCmp))
  let acc = ref(list{})
  s0->Belt.Set.forEach(x => {
    acc := Belt.List.add(acc.contents, x)
  })
  acc /* [6,5,3,2] */
  ```
*)

val reduceU: ('value, 'id) t -> 'a  -> ('a -> 'value -> 'a [@bs]) ->  'a

val reduce: ('value, 'id) t -> 'a  -> ('a -> 'value -> 'a ) ->  'a
(**
  Applies function `f` to each element of set in increasing order. Function `f` has two parameters: the item from the set and an “accumulator”, which starts with a value of `initialValue`. `reduce` returns the final value of the accumulator.

  ```res example
  let s0 = Belt.Set.fromArray([5,2,3,5,6], ~id=module(IntCmp))
  s0->Belt.Set.reduce(list{}, (acc, element) =>
    acc->Belt.List.add(element)
  ) /* [6,5,3,2] */
  ```
*)

val everyU: ('value, 'id) t -> ('value -> bool [@bs]) -> bool

val every: ('value, 'id) t -> ('value -> bool ) -> bool
(**
  Checks if all elements of the set satisfy the predicate. Order unspecified.

  ```res example
  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.Set.fromArray([2,4,6,8], ~id=module(IntCmp))
  s0->Belt.Set.every(isEven) /* true */
  ```
*)

val someU: ('value, 'id) t ->  ('value -> bool [@bs]) -> bool

val some: ('value, 'id) t ->  ('value -> bool ) -> bool
(**
  Checks if at least one element of the set satisfies the predicate.

  ```res example
  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.Set.fromArray([1,2,4,6,8], ~id=module(IntCmp))
  s0->Belt.Set.some(isOdd) /* true */
  ```
*)

val keepU: ('value, 'id) t ->  ('value -> bool [@bs]) -> ('value, 'id) t

val keep: ('value, 'id) t ->  ('value -> bool ) -> ('value, 'id) t
(**
  Returns the set of all elements that satisfy the predicate.

  ```res example
  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.Set.fromArray([1,2,3,4,5], ~id=module(IntCmp))
  let s1 = s0->Belt.Set.keep(isEven)

  s1->Belt.Set.toArray /* [2,4] */
  ```
*)

val partitionU: ('value, 'id) t -> ('value -> bool [@bs]) ->  ('value, 'id) t * ('value, 'id) t

val partition: ('value, 'id) t -> ('value -> bool) ->  ('value, 'id) t * ('value, 'id) t
(**
  Returns a pair of sets, where first is the set of all the elements of set that satisfy the predicate, and second is the set of all the elements of set that do not satisfy the predicate.

  ```res example
  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.Set.fromArray([1,2,3,4,5], ~id=module(IntCmp))
  let (s1, s2) = s0->Belt.Set.partition(isOdd)

  s1->Belt.Set.toArray /* [1,3,5] */
  s2->Belt.Set.toArray /* [2,4] */
  ```
*)

val size:  ('value, 'id) t -> int
(**
  Returns size of the set.

  ```res example
  let s0 = Belt.Set.fromArray([1,2,3,4], ~id=module(IntCmp))

  s0->Belt.Set.size /* 4 */
  ```
*)

val toArray: ('value, 'id) t -> 'value array
(**
  Returns array of ordered set elements.

  ```res example
  let s0 = Belt.Set.fromArray([3,2,1,5], ~id=module(IntCmp))

  s0->Belt.Set.toArray /* [1,2,3,5] */
  ```
*)

val toList: ('value, 'id) t -> 'value list
(**
  Returns list of ordered set elements.

  ```res example
  let s0 = Belt.Set.fromArray([3,2,1,5], ~id=module(IntCmp))

  s0->Belt.Set.toList /* [1,2,3,5] */
  ```
*)

val minimum: ('value, 'id) t -> 'value option
(**
  Returns minimum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.Set.make(~id=module(IntCmp))
  let s1 = Belt.Set.fromArray([3,2,1,5], ~id=module(IntCmp))

  s0->Belt.Set.minimum /* None */
  s1->Belt.Set.minimum /* Some(1) */
  ```
*)

val minUndefined: ('value, 'id) t -> 'value Js.undefined
(**
  Returns minimum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.Set.make(~id=module(IntCmp))
  let s1 = Belt.Set.fromArray([3,2,1,5], ~id=module(IntCmp))

  s0->Belt.Set.minUndefined /* undefined */
  s1->Belt.Set.minUndefined /* 1 */
  ```
*)

val maximum: ('value, 'id) t -> 'value option
(**
  Returns maximum value of the collection. `None` if collection is empty.

  ```res example
  let s0 = Belt.Set.make(~id=module(IntCmp))
  let s1 = Belt.Set.fromArray([3,2,1,5], ~id=module(IntCmp))

  s0->Belt.Set.maximum /* None */
  s1->Belt.Set.maximum /* Some(5) */
  ```
*)

val maxUndefined: ('value, 'id) t -> 'value Js.undefined
(**
  Returns maximum value of the collection. `undefined` if collection is empty.

  ```res example
  let s0 = Belt.Set.make(~id=module(IntCmp))
  let s1 = Belt.Set.fromArray([3,2,1,5], ~id=module(IntCmp))

  s0->Belt.Set.maxUndefined /* undefined */
  s1->Belt.Set.maxUndefined /* 5 */
  ```
*)

val get: ('value, 'id) t -> 'value -> 'value option
(**
  Returns the reference of the value which is equivalent to value using the comparator specifiecd by this collection. Returns `None` if element does not exist.

  ```res example
  let s0 = Belt.Set.fromArray([1,2,3,4,5], ~id=module(IntCmp))

  s0->Belt.Set.get(3) /* Some(3) */
  s0->Belt.Set.get(20) /* None */
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

val split: ('value, 'id) t -> 'value -> (('value, 'id) t  * ('value, 'id) t) * bool
(**
  Returns a tuple `((smaller, larger), present)`, `present` is true when element exist in set.

  ```res example
  let s0 = Belt.Set.fromArray([1,2,3,4,5], ~id=module(IntCmp))

  let ((smaller, larger), present) = s0->Belt.Set.split(3)

  present /* true */
  smaller->Belt.Set.toArray /* [1,2] */
  larger->Belt.Set.toArray /* [4,5] */

  ```
*)

(**/**)
val checkInvariantInternal: _ t -> unit
(**
   **raise** when invariant is not held
*)
(**/**)

(****************************************************************************)
(** Below are operations only when better performance needed,
  it is still safe API but more verbose.
  More API will be exposed by needs
*)

val getData: ('value, 'id) t  -> ('value, 'id) Belt_SetDict.t
(**
  **Advanced usage only**

  Returns the raw data (detached from comparator), but its type is still manifested, so that user can pass identity directly without boxing.
*)

val getId: ('value, 'id) t  -> ('value, 'id) id
(**
  **Advanced usage only**

  Returns the identity of set.
*)

val packIdData: id:('value, 'id) id -> data:('value, 'id) Belt_SetDict.t -> ('value, 'id) t
(**
  **Advanced usage only**

  Returns the packed collection.
*)
