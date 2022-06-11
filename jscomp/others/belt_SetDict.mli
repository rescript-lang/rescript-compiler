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
  This module separates identity from data. It is a bit more verbose but slightly more efficient due to the fact that there is no need to pack identity and data back after each operation.
*)

type ('value, 'identity) t
(**
  ```res prelude
  type t<'value, 'identity>
  ```

  `'value` is the element type

  `'identity` the identity of the collection
*)

type ('value, 'id) cmp = ('value, 'id) Belt_Id.cmp
(**
  ```res prelude
  type cmp<'value, 'id> = Belt.Id.cmp<'value, 'id>
  ```

  Type of compare function.
*)

val empty: ('value, 'id) t
(**
  ```res sig
  let empty: t<'value, 'id>
  ```

  ```res example
  let s0 = Belt.Set.Dict.empty
  ```
*)

val fromArray: 'value array -> cmp:('value, 'id) cmp -> ('value, 'id) t
(**
  ```res sig
  let fromArray: (array<'value>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>
  ```

  Creates new set from array of elements.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([1, 3, 2, 4], ~cmp=IntCmp.cmp)

  s0->Belt.Set.Dict.toArray /* [1, 2, 3, 4] */
  ```
*)

val fromSortedArrayUnsafe: 'value array -> ('value,'id) t
(**
  ```res sig
  let fromSortedArrayUnsafe: array<'value> => t<'value, 'id>
  ```

  The same as [fromArray][#fromarray] except it is after assuming the input array is already sorted.
*)

val isEmpty: _ t -> bool
(**
  ```res sig
  let isEmpty: t<'a, 'b> => bool
  ```

  Checks if set is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let empty = Belt.Set.Dict.fromArray([], ~cmp=IntCmp.cmp)
  let notEmpty = Belt.Set.Dict.fromArray([1], ~cmp=IntCmp.cmp)

  Belt.Set.Dict.isEmpty(empty) /* true */
  Belt.Set.Dict.isEmpty(notEmpty) /* false */
  ```
*)

val has: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> bool
(**
  ```res sig
  let has: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => bool
  ```

  Checks if an element exists in the set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let set = Belt.Set.Dict.fromArray([1, 4, 2, 5], ~cmp=IntCmp.cmp)

  set->Belt.Set.Dict.has(3, ~cmp=IntCmp.cmp) /* false */
  set->Belt.Set.Dict.has(1, ~cmp=IntCmp.cmp) /* true */
  ```
*)

val add: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> ('value, 'id) t
(**
  ```res sig
  let add: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => t<'value, 'id>
  ```

  Adds element to set. If element existed in set, value is unchanged.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.empty
  let s1 = s0->Belt.Set.Dict.add(1, ~cmp=IntCmp.cmp)
  let s2 = s1->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
  let s3 = s2->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
  s0->Belt.Set.Dict.toArray /* [] */
  s1->Belt.Set.Dict.toArray /* [1] */
  s2->Belt.Set.Dict.toArray /* [1, 2] */
  s3->Belt.Set.Dict.toArray /* [1,2 ] */
  s2 == s3 /* true */
  ```
*)

val mergeMany: ('value, 'id) t -> 'value array -> cmp:('value, 'id) cmp -> ('value, 'id) t
(**
  ```res sig
  let mergeMany: (t<'value, 'id>, array<'value>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>
  ```

  Adds each element of array to set. Unlike [add](#add), the reference of return value might be changed even if all values in array already exist in set

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let set = Belt.Set.Dict.empty

  let newSet = set->Belt.Set.Dict.mergeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
  newSet->Belt.Set.Dict.toArray /* [1, 2, 3, 4, 5] */
  ```
*)

val remove: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> ('value, 'id) t
(**
  ```res sig
  let remove: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => t<'value, 'id>
  ```

  Removes element from set. If element wasn't existed in set, value is unchanged.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([2, 3, 1, 4, 5], ~cmp=IntCmp.cmp)
  let s1 = s0->Belt.Set.Dict.remove(1, ~cmp=IntCmp.cmp)
  let s2 = s1->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)
  let s3 = s2->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)

  s1->Belt.Set.Dict.toArray /* [2,3,4,5] */
  s2->Belt.Set.Dict.toArray /* [2,4,5] */
  s2 == s3 /* true */
  ```
*)

val removeMany: ('value, 'id) t -> 'value array -> cmp:('value, 'id) cmp -> ('value, 'id) t
(**
  ```res sig
  let removeMany: (t<'value, 'id>, array<'value>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>
  ```

  Removes each element of array from set. Unlike [remove](#remove), the reference of return value might be changed even if any values in array not existed in set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let set = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

  let newSet = set->Belt.Set.Dict.removeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
  newSet->Belt.Set.Dict.toArray /* [] */
  ```
*)

val union: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> ('value, 'id) t
(**
  ```res sig
  let union: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>
  ```

  Returns union of two sets.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
  let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
  let union = Belt.Set.Dict.union(s0, s1, ~cmp=IntCmp.cmp)
  union->Belt.Set.Dict.toArray /* [1,2,3,4,5,6] */
  ```
*)

val intersect: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> ('value, 'id) t
(**
  ```res sig
  let intersect: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>
  ```

  Returns intersection of two sets.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
  let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
  let intersect = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
  intersect->Belt.Set.Dict.toArray /* [2,3,5] */
  ```
*)

val diff: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> ('value, 'id) t
(**
  ```res sig
  let diff: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>
  ```

  Returns elements from first set, not existing in second set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
  let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)

  let diff1 = Belt.Set.Dict.diff(s0, s1, ~cmp=IntCmp.cmp)
  let diff2 = Belt.Set.Dict.diff(s1, s0, ~cmp=IntCmp.cmp)

  diff1->Belt.Set.Dict.toArray /* [6] */
  diff2->Belt.Set.Dict.toArray /* [1,4] */
  ```
*)

val subset: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> bool
(**
  ```res sig
  let subset: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => bool
  ```

  Checks if second set is subset of first set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
  let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
  let s2 = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
  Belt.Set.Dict.subset(s2, s0, ~cmp=IntCmp.cmp) /* true */
  Belt.Set.Dict.subset(s2, s1, ~cmp=IntCmp.cmp) /* true */
  Belt.Set.Dict.subset(s1, s0, ~cmp=IntCmp.cmp) /* false */
  ```
*)

val cmp: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> int
(**
  ```res sig
  let cmp: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => int
  ```

  Total ordering between sets. Can be used as the ordering function for doing sets of sets. It compares size first and then iterates over each element following the order of elements.
*)

val eq: ('value, 'id) t -> ('value, 'id) t -> cmp:('value, 'id) cmp -> bool
(**
  ```res sig
  let eq: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => bool
  ```

  Checks if two sets are equal.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([5, 2, 3], ~cmp=IntCmp.cmp)
  let s1 = Belt.Set.Dict.fromArray([3, 2, 5], ~cmp=IntCmp.cmp)

  Belt.Set.Dict.eq(s0, s1, ~cmp=IntCmp.cmp) /* true */
  ```
*)

val forEachU: ('value, 'id) t -> ('value -> unit [@bs]) -> unit
(**
  ```res sig
  let forEachU: (t<'value, 'id>, (. 'value) => unit) => unit
  ```

  Same as [forEach](##forEach) but takes uncurried functon.
*)

val forEach: ('value, 'id) t -> ('value -> unit) -> unit
(**
  ```res sig
  let forEach: (t<'value, 'id>, 'value => unit) => unit
  ```

  Applies function `f` in turn to all elements of set in increasing order.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
  let acc = ref(list{})
  s0->Belt.Set.Dict.forEach(x => acc := Belt.List.add(acc.contents, x))
  acc /* [6,5,3,2] */
  ```
*)

val reduceU: ('value, 'id) t -> 'a -> ('a -> 'value -> 'a [@bs]) -> 'a
(**
  ```res sig
  let reduceU: (t<'value, 'id>, 'a, (. 'a, 'value) => 'a) => 'a
  ```
*)

val reduce: ('value, 'id) t -> 'a -> ('a -> 'value -> 'a) -> 'a
(**
  ```res sig
  let reduce: (t<'value, 'id>, 'a, ('a, 'value) => 'a) => 'a
  ```

  Applies function `f` to each element of set in increasing order. Function `f` has two parameters: the item from the set and an “accumulator”, which starts with a value of `initialValue`. `reduce` returns the final value of the accumulator.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
  s0->Belt.Set.Dict.reduce(list{}, (acc, element) => acc->Belt.List.add(element)) /* [6,5,3,2] */
  ```
*)

val everyU: ('value, 'id) t -> ('value -> bool [@bs]) -> bool
(**
  ```res sig
  let everyU: (t<'value, 'id>, (. 'value) => bool) => bool
  ```
*)

val every: ('value, 'id) t -> ('value -> bool) -> bool
(**
  ```res sig
  let every: (t<'value, 'id>, 'value => bool) => bool
  ```

  Checks if all elements of the set satisfy the predicate. Order unspecified.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.Set.Dict.fromArray([2, 4, 6, 8], ~cmp=IntCmp.cmp)
  s0->Belt.Set.Dict.every(isEven) /* true */
  ```
*)

val someU: ('value, 'id) t -> ('value -> bool [@bs]) -> bool
(**
  ```res sig
  let someU: (t<'value, 'id>, (. 'value) => bool) => bool
  ```
*)

val some: ('value, 'id) t -> ('value -> bool) -> bool
(**
  ```res sig
  let some: (t<'value, 'id>, 'value => bool) => bool
  ```

  Checks if at least one element of the set satisfies the predicate.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.Set.Dict.fromArray([1, 2, 4, 6, 8], ~cmp=IntCmp.cmp)
  s0->Belt.Set.Dict.some(isOdd) /* true */
  ```
*)

val keepU: ('value, 'id) t -> ('value -> bool [@bs]) -> ('value, 'id) t
(**
  ```res sig
  let keepU: (t<'value, 'id>, (. 'value) => bool) => t<'value, 'id>
  ```
*)

val keep: ('value, 'id) t -> ('value -> bool) -> ('value, 'id) t
(**
  ```res sig
  let keep: (t<'value, 'id>, 'value => bool) => t<'value, 'id>
  ```

  Returns the set of all elements that satisfy the predicate.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let isEven = x => mod(x, 2) == 0

  let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
  let s1 = s0->Belt.Set.Dict.keep(isEven)

  s1->Belt.Set.Dict.toArray /* [2,4] */
  ```
*)

val partitionU: ('value, 'id) t -> ('value -> bool [@bs]) -> ('value, 'id) t * ('value, 'id) t
(**
  ```res sig
  let partitionU: (t<'value, 'id>, (. 'value) => bool) => (t<'value, 'id>, t<'value, 'id>)
  ```
*)

val partition: ('value, 'id) t -> ('value -> bool) -> ('value, 'id) t * ('value, 'id) t
(**
  ```res sig
  let partition: (t<'value, 'id>, 'value => bool) => (t<'value, 'id>, t<'value, 'id>)
  ```

  Returns a pair of sets, where first is the set of all the elements of set that satisfy the predicate, and second is the set of all the elements of set that do not satisfy the predicate.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let isOdd = x => mod(x, 2) != 0

  let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
  let (s1, s2) = s0->Belt.Set.Dict.partition(isOdd)

  s1->Belt.Set.Dict.toArray /* [1,3,5] */
  s2->Belt.Set.Dict.toArray /* [2,4] */
  ```
*)

val size: ('value, 'id) t -> int
(**
  ```res sig
  let size: t<'value, 'id> => int
  ```

  Returns size of the set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

  s0->Belt.Set.Dict.size /* 4 */
  ```
*)

val toList: ('value, 'id) t -> 'value list
(**
  ```res sig
  let toList: t<'value, 'id> => list<'value>
  ```

  Returns list of ordered set elements.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

  s0->Belt.Set.Dict.toList /* [1,2,3,5] */
  ```
*)

val toArray: ('value, 'id) t -> 'value array
(**
  ```res sig
  let toArray: t<'value, 'id> => array<'value>
  ```

  Returns array of ordered set elements.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

  s0->Belt.Set.Dict.toArray /* [1,2,3,5] */
  ```
*)

val minimum: ('value, 'id) t -> 'value option
(**
  ```res sig
  let minimum: t<'value, 'id> => option<'value>
  ```

  Returns minimum value of the collection. `None` if collection is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.empty
  let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

  s0->Belt.Set.Dict.minimum /* None */
  s1->Belt.Set.Dict.minimum /* Some(1) */
  ```
*)

val minUndefined: ('value, 'id) t -> 'value Js.undefined
(**
  ```res sig
  let minUndefined: t<'value, 'id> => Js.undefined<'value>
  ```

  Returns minimum value of the collection. `undefined` if collection is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.empty
  let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

  s0->Belt.Set.Dict.minUndefined /* undefined */
  s1->Belt.Set.Dict.minUndefined /* 1 */
  ```
*)

val maximum: ('value, 'id) t -> 'value option
(**
  ```res sig
  let maximum: t<'value, 'id> => option<'value>
  ```

  Returns maximum value of the collection. `None` if collection is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.empty
  let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

  s0->Belt.Set.Dict.maximum /* None */
  s1->Belt.Set.Dict.maximum /* Some(5) */
  ```
*)

val maxUndefined: ('value, 'id) t -> 'value Js.undefined
(**
  ```res sig
  let maxUndefined: t<'value, 'id> => Js.undefined<'value>
  ```

  Returns maximum value of the collection. `undefined` if collection is empty.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.empty
  let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

  s0->Belt.Set.Dict.maxUndefined /* undefined */
  s1->Belt.Set.Dict.maxUndefined /* 5 */
  ```
*)

val get: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> 'value option
(**
  ```res sig
  let get: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => option<'value>
  ```

  Returns the reference of the value which is equivalent to value using the comparator specifiecd by this collection. Returns `None` if element does not exist.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

  s0->Belt.Set.Dict.get(3, ~cmp=IntCmp.cmp) /* Some(3) */
  s0->Belt.Set.Dict.get(20, ~cmp=IntCmp.cmp) /* None */
  ```
*)

val getUndefined: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> 'value Js.undefined
(**
  ```res sig
  let getUndefined: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => Js.undefined<'value>
  ```

  Same as [get](#get) but returns `undefined` when element does not exist.
*)

val getExn: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> 'value
(**
  ```res sig
  let getExn: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => 'value
  ```

  Same as [get](#get) but raise when element does not exist.
*)

val split: ('value, 'id) t -> 'value -> cmp:('value, 'id) cmp -> (('value, 'id) t * ('value, 'id) t) * bool
(**
  ```res sig
  let split: (
    t<'value, 'id>,
    'value,
    ~cmp: cmp<'value, 'id>,
  ) => ((t<'value, 'id>, t<'value, 'id>), bool)
  ```

  Returns a tuple `((smaller, larger), present)`, `present` is true when element exist in set.

  ```res example
  module IntCmp = Belt.Id.MakeComparable({
    type t = int
    let cmp = Pervasives.compare
  })

  let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

  let ((smaller, larger), present) = s0->Belt.Set.Dict.split(3, ~cmp=IntCmp.cmp)

  present /* true */
  smaller->Belt.Set.Dict.toArray /* [1,2] */
  larger->Belt.Set.Dict.toArray /* [4,5] */
  ```
*)

val checkInvariantInternal: _ t -> unit
(**
  ```res sig
  let checkInvariantInternal: t<'a, 'b> => unit
  ```

  **raise** when invariant is not held
*)
