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
  Collection functions for manipulating the `list` data structures, a singly-linked list.

  **Prefer Array** if you need any of the following:

  - Random access of element
  - Better interop with JavaScript
  - Better memory usage & performance.
*)

type 'a t = 'a list
(** `'a t` is compatible with built-in `list` type *)

val length : 'a t -> int
(**
  Returns the length of a list.

  ```res example
  Belt.List.length(list{1, 2, 3}) // 3
  ```
*)

val size : 'a t -> int
(** **See** [`length`](##length) *)

val head : 'a t -> 'a option
(**
  Returns `Some(value)` where `value` is the first element in the list, or
  `None` if `someList` is an empty list.

  ```res example
  Belt.List.head(list{}) // None
  Belt.List.head(list{1, 2, 3}) // Some(1)
  ```
*)

val headExn : 'a t -> 'a
(**
  Same as [head](#head), but raises an exception if `someList` is empty. Use
  with care.

  ```res example
  Belt.List.headExn(list{1, 2, 3}) // 1

  Belt.List.headExn(list{}) // Raises an Error
  ```
*)

val tail : 'a t -> 'a t option
(**
  Returns `None` if `someList` is empty, otherwise it returns `Some(tail)`
  where `tail` is everything except the first element of `someList`.

  ```res example
  Belt.List.tail(list{1, 2, 3}) // Some(list{2, 3})

  Belt.List.tail(list{}) // None
  ```
*)

val tailExn : 'a t -> 'a t
(**
  Same as [tail](#tail), but raises an exception if `someList` is empty. Use
  with care.

  ```res example
  Belt.List.tailExn(list{1, 2, 3}) // list{2, 3}

  Belt.List.tailExn(list{}) // Raises an Error
  ```
*)

val add : 'a t -> 'a -> 'a t
(**
  Adds `value` to the beginning of `someList`.

  ```res example
  Belt.List.add(list{2, 3}, 1) // list{1, 2, 3}

  Belt.List.add(list{"World", "!"}, "Hello") // list{"Hello", "World", "!"}
  ```
*)

val get : 'a t -> int -> 'a option
(**
  Return the nth element in `someList`, or `None` if `index` is larger than the
  length.

  ```res example
  let abc = list{"A", "B", "C"}

  abc->Belt.List.get(1) // Some("B")

  abc->Belt.List.get(4) // None
  ```
*)

val getExn : 'a t -> int -> 'a
(**
  Same as [get](#get), but raises an exception if `index` is larger than the
  length. Use with care.

  ```res example
  let abc = list{"A", "B", "C"}

  abc->Belt.List.getExn(1) // "B"

  abc->Belt.List.getExn(4) // Raises an Error
  ```
*)

val make : int -> 'a -> 'a t
(**
  Returns a list of length `numItems` with each element filled with value `v`. Returns an empty list if `numItems` is negative.

  ```res example
  Belt.List.make(3, 1) // list{1, 1, 1}
  ```
*)

val makeByU : int -> ((int -> 'a)[@bs]) -> 'a t
(** Uncurried version of [makeBy](#makeBy) *)

val makeBy : int -> (int -> 'a) -> 'a t
(**
Return a list of length `numItems` with element `i` initialized with `f(i)`.
Returns an empty list if `numItems` is negative.

```res example
Belt.List.makeBy(5, i => i) // list{0, 1, 2, 3, 4}

Belt.List.makeBy(5, i => i * i) // list{0, 1, 4, 9, 16}
```
*)

val shuffle : 'a t -> 'a t
(**
  Returns a new list in random order.

  ```res example
  Belt.List.shuffle(list{1, 2, 3}) // list{2, 1, 3}
  ```
*)

val drop : 'a t -> int -> 'a t option
(**
  Return a new list, dropping the first `n` elements. Returns `None` if `someList` has fewer than `n` elements.

  ```res example
  list{1, 2, 3}->Belt.List.drop(2) // Some(list{3})

  list{1, 2, 3}->Belt.List.drop(3) // Some(list{})

  list{1, 2, 3}->Belt.List.drop(4) // None
  ```
*)

val take : 'a t -> int -> 'a t option
(**
Returns a list with the first `n` elements from `someList`, or `None` if `someList` has fewer than `n` elements.

```res example
list{1, 2, 3}->Belt.List.take(1) // Some(list{1})

list{1, 2, 3}->Belt.List.take(2) // Some(list{1, 2})

list{1, 2, 3}->Belt.List.take(4) // None
```
*)

val splitAt : 'a t -> int -> ('a list * 'a list) option
(**
  Split the list `someList` at `index`. Returns `None` when the length of `someList` is less than `index`.

  ```res example
  list{"Hello", "World"}->Belt.List.splitAt(1) // Some((list{"Hello"}, list{"World"}))

  list{0, 1, 2, 3, 4}->Belt.List.splitAt(2) // Some((list{0, 1}, list{2, 3, 4}))
  ```
*)

val concat : 'a t -> 'a t -> 'a t
(**
  Returns the list obtained by adding `secondList` after `firstList`.

  ```res example
  Belt.List.concat(list{1, 2, 3}, list{4, 5}) // list{1, 2, 3, 4, 5}
  ```
*)

val concatMany : 'a t array -> 'a t
(**
  Returns the list obtained by concatenating all the lists in array `a`, in
  order.

  ```res example
  Belt.List.concatMany([list{1, 2, 3}, list{}, list{3}]) // list{1, 2, 3, 3}
  ```
*)

val reverseConcat : 'a t -> 'a t -> 'a t
(**
  Equivalent to writing: `concat(reverse(firstList, secondList)`

  ```res example
  Belt.List.reverseConcat(list{1, 2}, list{3, 4}) // list{2, 1, 3, 4}
  ```
*)

val flatten : 'a t t -> 'a t
(**
  Return the list obtained by concatenating all the lists in list `ls`, in order.

  ```res example
  Belt.List.flatten(list{list{1, 2, 3}, list{}, list{3}}) // list{1, 2, 3, 3}
  ```
*)

val mapU : 'a t -> (('a -> 'b)[@bs]) -> 'b t
(** Uncurried version of [map](#map). *)

val map : 'a t -> ('a -> 'b) -> 'b t
(**
  Returns a new list with `f` applied to each element of `someList`.

  ```res example
  list{1, 2}->Belt.List.map(x => x + 1) // list{3, 4}
  ```
*)

val zip : 'a t -> 'b t -> ('a * 'b) t
(**
  Returns a list of pairs from the two lists with the length of the shorter list.

  ```res example
  Belt.List.zip(list{1, 2}, list{3, 4, 5}) // list{(1, 3), (2, 4)}
  ```
*)

val zipByU : 'a t -> 'b t -> (('a -> 'b -> 'c)[@bs]) -> 'c t
(** Uncurried version of [zipBy](#zipBy). *)

val zipBy : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
(**
  **See:** [zip](#zip)

  ```res example
  Belt.List.zipBy(list{1, 2, 3}, list{4, 5}, (a, b) => 2 * a + b) // list{6, 9}
  ```
*)

val mapWithIndexU : 'a t -> ((int -> 'a -> 'b)[@bs]) -> 'b t
(** Uncurried version of [mapWithIndex](#mapWithIndex). *)

val mapWithIndex : 'a t -> (int -> 'a -> 'b) -> 'b t
(**
  Applies `f` to each element of `someList`.
  Function `f` takes two arguments: the index starting from 0 and the element from `someList`, in that order.

  ```res example
  list{1, 2, 3}->Belt.List.mapWithIndex((index, x) => index + x) // list{1, 3, 5}
  ```
*)

val fromArray : 'a array -> 'a t
(**
  Converts the given array to a list.

  ```res example
  Belt.List.fromArray([1, 2, 3]) // list{1, 2, 3}
  ```
*)

val toArray : 'a t -> 'a array
(**
  Converts the given list to an array.

  ```res example
  Belt.List.toArray(list{1, 2, 3}) // [1, 2, 3]
  ```
*)

(* type json = Js_json.t  *)

(* val toJson : 'a t -> ('a -> json  [@bs]) -> json *)
(* val fromJson : json -> (json -> 'a [@bs]) -> 'a t  *)

val reverse : 'a t -> 'a t
(**
  Returns a new list whose elements are those of `someList` in reversed order.

  ```res example
  Belt.List.reverse(list{1, 2, 3}) /* list{3, 2, 1} */
  ```
*)

val mapReverseU : 'a t -> (('a -> 'b)[@bs]) -> 'b t
(** Uncurried version of [mapReverse](#mapReverse). *)

val mapReverse : 'a t -> ('a -> 'b) -> 'b t
(**
  Equivalent to:

  ```res
  map(someList, f)->reverse
  ```

  ```res example
  list{3, 4, 5}->Belt.List.mapReverse(x => x * x) /* list{25, 16, 9} */
  ```
*)

val forEachU : 'a t -> (('a -> 'b)[@bs]) -> unit
(** Uncurried version of [forEach](#forEach). *)

val forEach : 'a t -> ('a -> 'b) -> unit
(**
  Call `f` on each element of `someList` from the beginning to end.
  `f` returns `unit`, so no new array is created. Use `forEach` when you are primarily concerned with repetitively creating side effects.

  ```res example
  Belt.List.forEach(list{"a", "b", "c"}, x => Js.log("Item: " ++ x))
  /*
    prints:
    Item: a
    Item: b
    Item: c
  */
  ```
*)

val forEachWithIndexU : 'a t -> ((int -> 'a -> 'b)[@bs]) -> unit
(** Uncurried version of [forEachWithIndex](#forEachWithIndex). *)

val forEachWithIndex : 'a t -> (int -> 'a -> 'b) -> unit
(**
  Call `f` on each element of `someList` from beginning to end.
  Function `f` takes two arguments: the index starting from 0 and the element from `someList`. `f` returns `unit`.

  ```res example
  Belt.List.forEachWithIndex(list{"a", "b", "c"}, (index, x) => {
    Js.log("Item " ++ Belt.Int.toString(index) ++ " is " ++ x)
  })
  /*
    prints:
    Item 0 is a
    Item 1 is b
    Item 2 is cc
  */
  ```
*)

val reduceU : 'a t -> 'b -> (('b -> 'a -> 'b)[@bs]) -> 'b
(** Uncurried version of [reduce](#reduce). *)

val reduce : 'a t -> 'b -> ('b -> 'a -> 'b) -> 'b
(**
  Applies `f` to each element of `someList` from beginning to end. Function `f` has two parameters: the item from the list and an “accumulator”, which starts with a value of `initialValue`. reduce returns the final value of the accumulator.

  ```res example
  list{1, 2, 3, 4}->Belt.List.reduce(0, (a, b) => a + b) /* 10 */

  /* same as */

  list{1, 2, 3, 4}->Belt.List.reduce(0, (acc, item) => acc + item) /* 10 */
  ```
*)

val reduceWithIndexU : 'a t -> 'b -> (('b -> 'a -> int -> 'b)[@bs]) -> 'b
(** Uncurried version of [reduceWithIndex](#reduceWithIndex). *)

val reduceWithIndex : 'a t -> 'b -> ('b -> 'a -> int -> 'b) -> 'b
(**
  Applies `f` to each element of `someList` from beginning to end. Function `f` has three parameters: the item from the list and an “accumulator”, which starts with a value of `initialValue` and the index of each element. `reduceWithIndex` returns the final value of the accumulator.

  ```res example
  list{1, 2, 3, 4}->Belt.List.reduceWithIndex(0, (acc, item, index) => acc + item + index) /* 16 */
  ```
*)

val reduceReverseU : 'a t -> 'b -> (('b -> 'a -> 'b)[@bs]) -> 'b
(** Uncurried version of [reduceReverse](#reduceReverse). *)

val reduceReverse : 'a t -> 'b -> ('b -> 'a -> 'b) -> 'b
(**
  Works like [reduce](#reduce), except that function `f` is applied to each
  item of `someList` from the last back to the first.

  ```res example
  list{1, 2, 3, 4}->Belt.List.reduceReverse(0, (a, b) => a + b) /* 10 */

  list{1, 2, 3, 4}->Belt.List.reduceReverse(10, (a, b) => a - b) /* 0 */

  list{1, 2, 3, 4}->Belt.List.reduceReverse(list{}, Belt.List.add) // list{1, 2, 3, 4}
  ```
*)

val mapReverse2U : 'a t -> 'b t -> (('a -> 'b -> 'c)[@bs]) -> 'c t
(** Uncurried version of [mapReverse2](#mapReverse2). *)

val mapReverse2 : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
(**
  Equivalent to: `zipBy(xs, ys, f)->reverse`

  ```res example

  Belt.List.mapReverse2(list{1, 2, 3}, list{1, 2}, (a, b) => a + b) // list{4, 2}
  ```
*)

val forEach2U : 'a t -> 'b t -> (('a -> 'b -> 'c)[@bs]) -> unit
(** Uncurried version of [forEach2](#forEach2). *)

val forEach2 : 'a t -> 'b t -> ('a -> 'b -> 'c) -> unit
(**
  Stops at the length of the shorter list.

  ```res example
  Belt.List.forEach2(list{"Z", "Y"}, list{"A", "B", "C"}, (x, y) => Js.log2(x, y))

  /*
    prints:
    "Z" "A"
    "Y" "B"
  */
  ```
*)

val reduce2U : 'b t -> 'c t -> 'a -> (('a -> 'b -> 'c -> 'a)[@bs]) -> 'a
(** Uncurried version of [reduce2](#reduce2). *)

val reduce2 : 'b t -> 'c t -> 'a -> ('a -> 'b -> 'c -> 'a) -> 'a
(**
  Applies `f` to each element of `firstList` and `secondList` from beginning to end. Stops with the shorter list. Function `f` has three parameters: an “accumulator” which starts with a value of `initialValue`, an item from `firstList`, and an item from `secondList`. `reduce2` returns the final value of the accumulator.

  ```res example
  Belt.List.reduce2(list{1, 2, 3}, list{4, 5}, 0, (acc, x, y) => acc + x * x + y) /* 0 + (1 * 1 + 4) + (2 * 2 + 5) */
  ```
*)

val reduceReverse2U : 'a t -> 'b t -> 'c -> (('c -> 'a -> 'b -> 'c)[@bs]) -> 'c
(** Uncurried version of [reduceReverse2](#reduceReverse2). *)

val reduceReverse2 : 'a t -> 'b t -> 'c -> ('c -> 'a -> 'b -> 'c) -> 'c
(**
  Applies `f` to each element of `firstList` and `secondList` from end to
  beginning. Stops with the shorter list. Function `f` has three parameters: an
  “accumulator” which starts with a value of init, an item from `firstList`,
  and an item from `secondList`. `reduce2` returns the final value of the
  accumulator.

  ```res example
  Belt.List.reduceReverse2(list{1, 2, 3}, list{4, 5}, 0, (acc, x, y) => acc + x * x + y) /*  + (1 * 1 + 4) + (2 * 2 + 5) */
  ```
*)

val everyU : 'a t -> (('a -> bool)[@bs]) -> bool
(** Uncurried version of [every](#every). *)

val every : 'a t -> ('a -> bool) -> bool
(**
  Returns `true` if all elements satisfy `pred`, where `pred` is a predicate: a function taking an element and returning a bool.

  ```res example
  let isBelow10 = value => value < 10

  list{1, 9, 8, 2}->Belt.List.every(isBelow10) /* true */

  list{1, 99, 8, 2}->Belt.List.every(isBelow10) /* false */
  ```
*)

val someU : 'a t -> (('a -> bool)[@bs]) -> bool
(** Uncurried version of [some](#some). *)

val some : 'a t -> ('a -> bool) -> bool
(**
  Returns `true` if at least _one_ of the elements in `someList` satisfies
  `pred`, where `pred` is a predicate: a function taking an element and
  returning a bool.

  ```res example
  let isAbove100 = value => value > 100

  list{101, 1, 2, 3}->Belt.List.some(isAbove100) /* true */

  list{1, 2, 3, 4}->Belt.List.some(isAbove100) /* false */
  ```
*)

val every2U : 'a t -> 'b t -> (('a -> 'b -> bool)[@bs]) -> bool
(** Uncurried version of [every2](#every2). *)

val every2 : 'a t -> 'b t -> ('a -> 'b -> bool) -> bool
(**
  Returns `true` if predicate `pred(a, b)` is `true` for all pairs of elements
  up to the shorter length (i.e. `min(length(firstList), length(secondList))`)

  ```res example
  Belt.List.every2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) /* true */

  Belt.List.every2(list{}, list{1}, (a, b) => a > b) /* true */

  Belt.List.every2(list{2, 3}, list{1}, (a, b) => a > b) /* true */

  Belt.List.every2(list{0, 1}, list{5, 0}, (a, b) => a > b) /* false */
  ```
*)

val some2U : 'a t -> 'b t -> (('a -> 'b -> bool)[@bs]) -> bool
(** Uncurried version of [some2](#some2). *)

val some2 : 'a t -> 'b t -> ('a -> 'b -> bool) -> bool
(**
  Returns `true` if predicate `pred(a, b)` is true for any pair of elements up
  to the shorter length (i.e. `min(length(firstList), length(secondList))`)

  ```res example
  Belt.List.some2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) /* true */

  Belt.List.some2(list{}, list{1}, (a, b) => a > b) /* false */

  Belt.List.some2(list{2, 3}, list{1}, (a, b) => a > b) /* true */

  Belt.List.some2(list{0, 1}, list{5, 0}, (a, b) => a > b) /* true */
  ```
*)

val cmpByLength : 'a t -> 'a t -> int
(**
  Compare two lists solely by length. Returns `-1` if `length(firstList)` is
  less than `length(secondList)`, `0` if `length(firstList)` equals
  `length(secondList)`, and `1` if `length(firstList)` is greater than
  `length(secondList)`.

  ```res example
  Belt.List.cmpByLength(list{1, 2}, list{3, 4, 5, 6}) /* -1 */

  Belt.List.cmpByLength(list{1, 2, 3}, list{4, 5, 6}) /* = 0 */

  Belt.List.cmpByLength(list{1, 2, 3, 4}, list{5, 6}) /* = 1 */
  ```
*)

val cmpU : 'a t -> 'a t -> (('a -> 'a -> int)[@bs]) -> int
(** Uncurried version of [cmp](#cmp). *)

val cmp : 'a t -> 'a t -> ('a -> 'a -> int) -> int
(**
  Compare elements one by one `compareFn(a, b)`. `compareFn` returns a negative number if `a` is "less than" `b`, zero if `a` is "equal to" `b`, a positive number if `a` is "greater than" `b`.

  The comparison returns the first non-zero result of `compareFn`, or zero if `compareFn` returns zero for all `a` and `b`.

  If all items have compared equal, but `firstList` is exhausted first, return `-1`. (`firstList` is shorter).
  If all items have compared equal, but `secondList` is exhausted first, return `1` (`firstList` is longer).

  ```res example
  Belt.List.cmp(list{3}, list{3, 7}, (a, b) => compare(a, b)) /* (-1) */

  Belt.List.cmp(list{5, 3}, list{5}, (a, b) => compare(a, b)) /* 1 */

  Belt.List.cmp(list{1, 3, 5}, list{1, 4, 2}, (a, b) => compare(a, b)) /* (-1) */

  Belt.List.cmp(list{1, 3, 5}, list{1, 2, 3}, (a, b) => compare(a, b)) /* 1 */

  Belt.List.cmp(list{1, 3, 5}, list{1, 3, 5}, (a, b) => compare(a, b)) /* 0 */
  ```

  **Please note:** The total ordering of List is different from Array,
  for Array, we compare the length first and, only if the lengths are equal, elements one by one.
  For lists, we just compare elements one by one.
*)

val eqU : 'a t -> 'a t -> (('a -> 'a -> bool)[@bs]) -> bool
(** Uncurried version of [eq](#eq). *)

val eq : 'a t -> 'a t -> ('a -> 'a -> bool) -> bool
(**
  Check equality of `firstList` and `secondList` using `eqElem` for equality on
  elements, where `eqElem` is a function that returns `true` if items `x` and
  `y` meet some criterion for equality, `false` otherwise. eq `false` if length
  of `firstList` and `secondList` are not the same.

  ```res example
  Belt.List.eq(list{1, 2, 3}, list{1, 2}, (a, b) => a == b) /* false */

  Belt.List.eq(list{1, 2}, list{1, 2}, (a, b) => a == b) /* true */

  Belt.List.eq(list{1, 2, 3}, list{(-1), (-2), (-3)}, (a, b) => abs(a) == abs(b)) /* true */
  ```
*)

val hasU : 'a t -> 'b -> (('a -> 'b -> bool)[@bs]) -> bool
(** Uncurried version of [has](#has). *)

val has : 'a t -> 'b -> ('a -> 'b -> bool) -> bool
(**
  Returns `true` if the list contains at least one element for which
  `eqFunction(x)` returns true.

  ```res example
  list{1, 2, 3}->Belt.List.has(2, (a, b) => a == b) /* true */

  list{1, 2, 3}->Belt.List.has(4, (a, b) => a == b) /* false */

  list{(-1), (-2), (-3)}->Belt.List.has(2, (a, b) => abs(a) == abs(b)) /* true */
  ```
*)

val getByU : 'a t -> (('a -> bool)[@bs]) -> 'a option
(** Uncurried version of [getBy](#getBy). *)

val getBy : 'a t -> ('a -> bool) -> 'a option
(**
  Returns `Some(value)` for the first value in `someList` that satisfies the
  predicate function `pred`. Returns `None` if no element satisfies the function.

  ```res example
  Belt.List.getBy(list{1, 4, 3, 2}, x => x > 3) /* Some(4) */

  Belt.List.getBy(list{1, 4, 3, 2}, x => x > 4) /* None */
  ```
*)

val keepU : 'a t -> (('a -> bool)[@bs]) -> 'a t
(** Uncurried version of [keep](#keep). *)

val keep : 'a t -> ('a -> bool) -> 'a t
(**
  Returns a list of all elements in `someList` which satisfy the predicate function `pred`.

  ```res example
  let isEven = x => mod(x, 2) == 0

  Belt.List.keep(list{1, 2, 3, 4}, isEven) /* list{2, 4} */

  Belt.List.keep(list{None, Some(2), Some(3), None}, Belt.Option.isSome) /* list{Some(2), Some(3)} */
  ```
*)

val filter : 'a t -> ('a -> bool) -> 'a t
  [@@deprecated
    "This function will soon be deprecated. Please, use `List.keep` instead."]
(**
  Returns a list of all elements in `someList` which satisfy the predicate function `pred`.

  ```res example
  let isEven = x => mod(x, 2) == 0

  Belt.List.filter(list{1, 2, 3, 4}, isEven) /* list{2, 4} */

  Belt.List.filter(list{None, Some(2), Some(3), None}, Belt.Option.isSome) /* list{Some(2), Some(3)} */
  ```
*)

val keepWithIndexU : 'a t -> (('a -> int -> bool)[@bs]) -> 'a t
(** Uncurried version of [keepWithIndex](#keepWithIndex). *)

val keepWithIndex : 'a t -> ('a -> int -> bool) -> 'a t
(**
  Returns a list of all elements in `someList` which satisfy the predicate function `pred`.

  ```res example
  let isEven = x => mod(x, 2) == 0

  Belt.List.keepWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) /* list{1, 3} */
  ```
*)

val filterWithIndex : 'a t -> ('a -> int -> bool) -> 'a t
  [@@deprecated
    "This function will soon be deprecated. Please, use `List.keepWithIndex` \
     instead."]
(**
  Returns a list of all elements in `someList` which satisfy the predicate function `pred`.

  ```res example
  let isEven = x => mod(x, 2) == 0

  Belt.List.filterWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) /* list{1, 3} */
  ```
*)

val keepMapU : 'a t -> (('a -> 'b option)[@bs]) -> 'b t
(** Uncurried version of [keepMap](#keepMap). *)

val keepMap : 'a t -> ('a -> 'b option) -> 'b t
(**
  Applies `f` to each element of `someList`. If `f(x)` returns `Some(value)`, then `value` is _kept_ in the resulting list.
  If `f(x)` returns `None`, the element is _not_ retained in the result.

  ```res example
  let isEven = x => mod(x, 2) == 0

  list{1, 2, 3, 4}
  ->Belt.List.keepMap(x =>
      if (isEven(x)) {
        Some(x)
      } else {
        None
      }
    ) /* list{2, 4} */

  list{Some(1), Some(2), None}->Belt.List.keepMap(x => x) /* list{1, 2} */
  ```
*)

val partitionU : 'a t -> (('a -> bool)[@bs]) -> 'a t * 'a t
(** Uncurried version of [partition](#partition). *)

val partition : 'a t -> ('a -> bool) -> 'a t * 'a t
(**
  Creates a pair of lists; the first list consists of all elements of `someList` that satisfy the predicate function `pred`; the second list consists of all elements of `someList` that _do not_ satisfy `pred.

  In other words:

  ```res
  (elementsThatSatisfies, elementsThatDoesNotSatisfy)
  ```

  ```res example
  Belt.List.partition(list{1, 2, 3, 4}, x => x > 2) /* (list{3, 4}, list{1, 2}) */
  ```
*)

val unzip : ('a * 'b) t -> 'a t * 'b t
(**
  Takes a list of pairs and creates a pair of lists. The first list contains all the first items of the pairs; the second list contains all the second items.

  ```res example
  Belt.List.unzip(list{(1, 2), (3, 4)}) /* (list{1, 3}, list{2, 4}) */

  Belt.List.unzip(list{("H", "W"), ("e", "o"), ("l", "r"), ("l", "l"), ("o", "d"), (" ", "!")})
  /* (list{"H", "e", "l", "l", "o", " "}, list{"W", "o", "r", "l", "d", "!"}) */
  ```
*)

val getAssocU : ('a * 'c) t -> 'b -> (('a -> 'b -> bool)[@bs]) -> 'c option
(** Uncurried version of [getAssoc](#getAssoc). *)

val getAssoc : ('a * 'c) t -> 'b -> ('a -> 'b -> bool) -> 'c option
(**
  Return the second element of a pair in `someList` where the first element equals `k` as per the predicate function `eqFunction`, or `None` if not found.

  ```res example
  list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.getAssoc(3, (a, b) => a == b) /* Some("c") */

  list{(9, "morning"), (15, "afternoon"), (22, "night")}
  ->Belt.List.getAssoc(15, (k, item) => k /* 15 */ == item /* 9, 5, 22 */)
  /* Some("afternoon") */
  ```
*)

val hasAssocU : ('a * 'c) t -> 'b -> (('a -> 'b -> bool)[@bs]) -> bool
(** Uncurried version of [hasAssoc](#hasAssoc). *)

val hasAssoc : ('a * 'c) t -> 'b -> ('a -> 'b -> bool) -> bool
(**
  Returns `true` if there is a pair in `someList` where the first element equals `k` as per the predicate function `eqFunction`.

  ```res example
  list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.hasAssoc(1, (a, b) => a == b) /* true */

  list{(9, "morning"), (15, "afternoon"), (22, "night")}
  ->Belt.List.hasAssoc(25, (k, item) => k /* 25 */ == item /* 9, 5, 22 */) /* false */
  ```
*)

val removeAssocU : ('a * 'c) t -> 'b -> (('a -> 'b -> bool)[@bs]) -> ('a * 'c) t
(** Uncurried version of [removeAssoc](#removeAssoc). *)

val removeAssoc : ('a * 'c) t -> 'b -> ('a -> 'b -> bool) -> ('a * 'c) t
(**
  Return a list after removing the first pair whose first value is `k` per the equality predicate `eqFunction`; if not found, return a new list identical to `someList`.

  ```res example
  list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.removeAssoc(1, (a, b) => a == b) /* list{(2, "b"), (3, "c")} */

  list{(9, "morning"), (15, "afternoon"), (22, "night")}
  ->Belt.List.removeAssoc(9, (k, item) => k /* 9 */ == item /* 9, 5, 22 */)
  /* list{(15, "afternoon"), (22, "night")} */
  ```
*)

val setAssocU :
  ('a * 'c) t -> 'a -> 'c -> (('a -> 'a -> bool)[@bs]) -> ('a * 'c) t
(** Uncurried version of [setAssoc](#setAssoc). *)

val setAssoc : ('a * 'c) t -> 'a -> 'c -> ('a -> 'a -> bool) -> ('a * 'c) t
(**
  If `k` exists in `someList` by satisfying the `eqFunction` predicate, return a new list with the key and value replaced by the new `k` and `v`; otherwise, return a new list with the pair `k`, `v` added to the head of `someList`.

  ```res example
  list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.setAssoc(2, "x", (a, b) => a == b) /* list{(1, "a"), (2, "x"), (3, "c")} */

  list{(1, "a"), (3, "c")}->Belt.List.setAssoc(2, "b", (a, b) => a == b) /* list{(2, "b"), (1, "a"), (3, "c")} */

  list{(9, "morning"), (3, "morning?!"), (22, "night")}
  ->Belt.List.setAssoc(15, "afternoon", (a, b) => mod(a, 12) == mod(b, 12))
  /* list{(9, "morning"), (15, "afternoon"), (22, "night")} */
  ```

  **Please note**

  In the last example, since: `15 mod 12` equals `3 mod 12`

  Both the key _and_ the value are replaced in the list.
*)

val sortU : 'a t -> (('a -> 'a -> int)[@bs]) -> 'a t
(** Uncurried version of [sort](#sort). *)

val sort : 'a t -> ('a -> 'a -> int) -> 'a t
(**
  Returns a sorted list.

  ```res example
  Belt.List.sort(list{5, 4, 9, 3, 7}, (a, b) => a - b) // list{3, 4, 5, 7, 9}
  ```
*)
