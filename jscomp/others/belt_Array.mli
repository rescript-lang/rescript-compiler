(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(* Adapted significantly by Authors of ReScript *)


(** Utililites for `Array` functions.

### Note about index syntax

Code like `arr[0]` does *not* compile to JavaScript `arr[0]`. Reason transforms the `[]` index syntax into a function: `Array.get(arr, 0)`. By default, this uses the default standard library's `Array.get` function, which may raise an exception if the index isn't found. If you `open Belt`, it will use the `Belt.Array.get` function which returns options instead of raising exceptions. [See this for more information](../belt.mdx#array-access-runtime-safety).
*)

type 'a t = 'a array

external length: 'a t -> int = "%array_length"
(** return the size of the array

```res example
// Returns 1
Belt.Array.length(["test"])
```
*)

external size: 'a t -> int = "%array_length"
(** **See** [`length`]() *)

val get: 'a t -> int -> 'a option
(**
   If `i <= 0 <= length(arr)` returns `Some(value)` where `value` is the item at index `i`.
  If `i` is out of range returns `None`.
  ```
  Belt.Array.get(["a", "b", "c"], 0) == Some("a")
  Belt.Array.get(["a", "b", "c"], 3) == None
  Belt.Array.get(["a", "b", "c"], -1) == None
  ```
*)

val getExn: 'a t -> int -> 'a
(**
  Raise an exception if `i` is out of range.
  Otherwise return the value at index `i` in `arr`.
*)

external getUnsafe: 'a t -> int -> 'a = "%array_unsafe_get"
(**
  `getUnsafe(arr, i)`

  **Unsafe**

  no bounds checking; this would cause type error if `i` does not stay within range
*)

external getUndefined: 'a t -> int -> 'a Js.undefined = "%array_unsafe_get"
(**
  `getUndefined(arr, i)`

  It does the samething in the runtime as [`getUnsafe`]();
  it is _type safe_ since the return type still track whether it is
  in range or not
*)

val set: 'a t -> int -> 'a -> bool
(**
  `set(arr, n, x)` modifies `arr` in place; it replaces the nth element of `arr` with `x`.
  Returning `false` means not updated due to out of range.
*)

val setExn: 'a t -> int -> 'a -> unit
(**
  `setExn(arr, i, x)` raise an exception if `i` is out of range.
*)

external setUnsafe: 'a t -> int -> 'a -> unit = "%array_unsafe_set"

val shuffleInPlace: 'a t -> unit
(** `shuffleInPlace(arr)` randomly re-orders the items in `arr` *)

val shuffle: 'a t -> 'a t
(** Returns a fresh array with items in original array randomly shuffled. *)

val reverseInPlace: 'a t -> unit
(**
  `reverseInPlace(arr)` reverses items in `arr` in place.

  ```res example
  let arr = [10, 11, 12, 13, 14]

  let () = Belt.Array.reverseInPlace(arr)

  arr == [14, 13, 12, 11, 10]
  ```
*)

val reverse: 'a t -> 'a t
(**
  `reverse(arr)` returns a fresh array with items in arr in reverse order.

  ```res example
  Belt.Array.reverse([10, 11, 12, 13, 14]) == [14, 13, 12, 11, 10]
  ```
*)

external makeUninitialized: int -> 'a Js.undefined array = "Array" [@@bs.new]
(**
  `makeUninitialized(n)` creates an array of length `n` filled with the undefined value. You must specify the type of data that will eventually fill the array.

  ```res example
  let arr: array<Js.undefined<string>> = Belt.Array.makeUninitialized(5)

  Belt.Array.getExn(arr, 0) == Js.undefined
  ```
*)

external makeUninitializedUnsafe: int -> 'a t = "Array" [@@bs.new]
(**
  **Unsafe**

  ```res example
  let arr = Belt.Array.makeUninitializedUnsafe(5)

  Js.log(Belt.Array.getExn(arr, 0)) // undefined

  Belt.Array.setExn(arr, 0, "example")

  Js.log(Belt.Array.getExn(arr, 0) == "example")
  ```
*)


val make: int -> 'a  -> 'a t
(**
  `make(n, e)` return an array of size `n` filled with value `e`.
  Returns an empty array when `n` is negative.
*)

val range: int -> int -> int array
(**
  `range(start, finish)` create an inclusive array.

  ```res example
  Belt.Array.range(0, 3) == [0, 1, 2, 3]

  Belt.Array.range(3, 0) == []

  Belt.Array.range(3, 3) == [3]
  ```
*)

val rangeBy: int -> int -> step:int -> int array
(**
  `rangeBy(start, finish, ~step)`

  Returns empty array when step is 0 or negative. It also return an empty array when `start > finish`.

  ```res example
  Belt.Array.rangeBy(0, 10, ~step=3) == [0, 3, 6, 9]

  Belt.Array.rangeBy(0, 12, ~step=3) == [0, 3, 6, 9, 12]

  Belt.Array.rangeBy(33, 0, ~step=1) == []

  Belt.Array.rangeBy(33, 0, ~step=-1) == []

  Belt.Array.rangeBy(3, 12, ~step=-1) == []

  Belt.Array.rangeBy(3, 3, ~step=0) == []

  Belt.Array.rangeBy(3, 3, ~step=1) == [3]
  ```
*)

val makeByU: int -> (int -> 'a [@bs]) -> 'a t
val makeBy: int -> (int -> 'a ) -> 'a t
(**
  `makeBy(n, f)`

  Return an empty array when n is negative return an array of size n populated by `f(i)` start from `0` to `n - 1`.

  ```res example
  Belt.Array.makeBy(5, (i) => i) == [0, 1, 2, 3, 4]

  Belt.Array.makeBy(5, (i) => i * i) == [0, 1, 4, 9, 16]
  ```
*)

val makeByAndShuffleU: int -> (int -> 'a [@bs]) -> 'a t
val makeByAndShuffle: int -> (int -> 'a ) -> 'a t
(**
  Equivalent to `shuffle(makeBy(n, f))`
*)


val zip: 'a t -> 'b array -> ('a * 'b) array
(**
  `zip(a, b)`

  Create an array of pairs from corresponding elements of a and b. Stop with the shorter array.

  ```res example
  Belt.Array.zip([1, 2], [3, 4, 5]) == [(1, 3), (2, 4)]
  ```
*)


val zipByU: 'a t -> 'b array -> ('a -> 'b -> 'c [@bs]) -> 'c array
val zipBy: 'a t -> 'b array -> ('a -> 'b -> 'c ) -> 'c array
(**
  `zipBy(xs, ys, f)`

  Create an array by applying `f` to corresponding elements of `xs` and `ys`. Stops with shorter array.

  Equivalent to `map(zip(xs, ys), ((a, b)) => f(a, b))`

  ```res example
  Belt.Array.zipBy([1, 2, 3], [4, 5], (a, b) => 2 * a + b) == [6, 9]
  ```
*)

val unzip: ('a * 'b) array -> 'a t * 'b array
(**
  `unzip(a)` takes an array of pairs and creates a pair of arrays. The first array contains all the first items of the pairs; the second array contains all the second items.

  ```res example
  Belt.Array.unzip([(1, 2), (3, 4)]) == ([1, 3], [2, 4])

  Belt.Array.unzip([(1, 2), (3, 4), (5, 6), (7, 8)]) == ([1, 3, 5, 7], [2, 4, 6, 8])
  ```
*)

val concat: 'a t -> 'a t -> 'a t
(**
  `concat(xs, ys)`

  Returns a fresh array containing the concatenation of the arrays `v1` and `v2`;so even if `v1` or `v2` is empty; it can not be shared

  ```res example
  Belt.Array.concat([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]

  Belt.Array.concat([], ["a", "b", "c"]) == ["a", "b", "c"]
  ```
*)

val concatMany: 'a t array -> 'a t
(**
  `concatMany(xss)`

  Returns a fresh array as the concatenation of `xss` (an array of arrays)

  ```res example
  Belt.Array.concatMany([[1, 2, 3], [4, 5, 6], [7, 8]]) == [1, 2, 3, 4, 5, 6, 7, 8]
  ```
*)

val slice: 'a t -> offset:int -> len:int -> 'a t
(**
  `slice(xs, offset, len)` creates a new array with the len elements of `xs`
  starting at `offset` for `offset` can be negative;and is evaluated as
  `length(xs) - offset(slice, xs) - 1(1)` means get the last element as a
  singleton array `slice(xs, ~-len, len)` will return a copy of the array if the
  array does not have enough data; `slice` extracts through the end of sequence.

  if `len` is negative; returns the empty array.

  ```res example
  Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=2, ~len=3) == [12, 13, 14]

  Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=-4, ~len=3) == [13, 14, 15]

  Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=4, ~len=9) == [14, 15, 16]
  ```
*)

val sliceToEnd: 'a t -> int -> 'a t
(**
  `sliceToEnd(xs, offset)` creates a new array with the elements of `xs` starting at `offset`

  `offset` can be negative; and is evaluated as `length(xs) - offset(sliceToEnd, xs) - 1` means get the last element as a singleton array

  `sliceToEnd(xs, 0)` will return a copy of the array

  ```res example
  Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], 2) == [12, 13, 14, 15, 16]

  Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], -4) == [13, 14, 15, 16]
  ```
*)


external copy : 'a t -> (_ [@bs.as 0]) -> 'a t = "slice" [@@bs.send]
(**
  `copy(a)`

  Returns a copy of a; that is; a fresh array containing the same elements as a.
*)

val fill: 'a t -> offset:int -> len:int -> 'a -> unit
(**
  `fill(arr, ~offset, ~len, x)`

  Modifies `arr` in place, storing `x` in elements number `offset` to `offset + len - 1`.
  `offset` can be negative; and is evaluated as `length(arr - offset)`

  `fill(arr, ~offset=-1, ~len=1)` means fill the last element, if the array does not have enough data; `fill` will ignore it

  ```res example
  let arr = Belt.Array.makeBy(5, (i) => i)

  Belt.Array.fill(arr, ~offset=2, ~len=2, 9)

  arr == [0, 1, 9, 9, 4]

  Belt.Array.fill(arr, ~offset=7, ~len=2, 8)

  arr == [0, 1, 9, 9, 4]
*)

val blit:
  src:'a t -> srcOffset:int -> dst:'a t -> dstOffset:int -> len:int -> unit
(**
  `blit(~src=v1, ~srcOffset=o1, ~dst=v2, ~dstOffset=o2, ~len)`

  copies `len` elements from array `v1`;starting at element number `o1`;to array `v2`, starting at element number `o2`.

  It works correctly even if `v1` and `v2` are the same array;and the source and destination chunks overlap.

  `offset` can be negative; `-1` means `len - 1`; if `len + offset` is still negative;it will be set as 0

  For each of the examples;presume that `v1 == [10, 11, 12, 13, 14, 15, 16, 17]` and `v2 == [20, 21, 22, 23, 24, 25, 26, 27]`. The result shown is the content of the destination array.

  ```res example
  let v1 = [10, 11, 12, 13, 14, 15, 16, 17]
  let v2 = [20, 21, 22, 23, 24, 25, 26, 27]

  Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v2, ~dstOffset=2, ~len=3)
  v2 == [20, 21, 14, 15, 16, 25, 26, 27]

  Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v1, ~dstOffset=2, ~len=3)
  v1 == [10, 11, 14, 15, 16, 15, 16, 17]
  ```
*)

val blitUnsafe:
  src:'a t -> srcOffset:int -> dst:'a t -> dstOffset:int -> len:int -> unit
(**
  Unsafe blit without bounds checking.
*)

val forEachU: 'a t ->  ('a -> unit [@bs]) -> unit
val forEach: 'a t ->  ('a -> unit ) -> unit
(**
  `forEach(xs, f)`

  Call `f` on each element of `xs` from the beginning to end. `f` returns `unit`;so no new array is created. Use `forEach` when you are primarily concerned with repetitively creating side effects.

  ```res example
  Belt.Array.forEach(["a", "b", "c"], x => Js.log("Item: " ++ x))

  /*
    prints:
    Item: a
    Item: b
    Item: c
  */
  let total = ref(0)

  Belt.Array.forEach([1, 2, 3, 4], x => total := total.contents + x)

  total.contents == 1 + 2 + 3 + 4
  ```
*)

val mapU: 'a t ->  ('a -> 'b [@bs]) -> 'b array
val map: 'a t ->  ('a -> 'b ) -> 'b array
(**
  `map(xs, f)`

  Returns a new array by calling `f` for each element of `xs` from the beginning to end.

  ```res example
  Belt.Array.map([1, 2], (x) => x + 1) == [3, 4]
  ```
*)

val flatMapU: 'a t -> ('a -> 'b array [@bs]) -> 'b array
val flatMap: 'a t -> ('a -> 'b array) -> 'b array
(**
  `flatMap(xs, f)`

  **Returns** a new array by calling `f` for each element of `xs` from
  the beginning to end, concatenating the results.

  ```res example
  flatMap([1, 2], x => [x + 10, x + 20]) == [11, 21, 12, 22]
  ```
*)

val getByU: 'a t -> ('a -> bool [@bs]) -> 'a option
val getBy: 'a t -> ('a -> bool) -> 'a option
(**
  `getBy(xs, p)`

  Returns `Some(value)` for the first value in `xs` that satisifies the predicate function `p`; returns `None` if no element satisifies the function.

  ```res example
  Belt.Array.getBy([1, 4, 3, 2], (x) => mod(x, 2) == 0) == Some(4)
  Belt.Array.getBy([15, 13, 11], (x) => mod(x, 2) == 0) == None
  ```
*)

val getIndexByU: 'a t -> ('a -> bool [@bs]) -> int option
val getIndexBy: 'a t -> ('a -> bool) -> int option
(**
  `getIndexBy(xs, p)` returns `Some(index)` for the first value in `xs` that satisifies the predicate function `p`;
  returns `None` if no element satisifies the function.

  ```res example
  Belt.Array.getIndexBy([1, 4, 3, 2], (x) => mod(x, 2) == 0) == Some(1)
  Belt.Array.getIndexBy([15, 13, 11], (x) => mod(x, 2) == 0) == None
  ```
*)

val keepU: 'a t -> ('a -> bool [@bs]) -> 'a t
val keep: 'a t -> ('a -> bool ) -> 'a t
(**
  `keep(xs, p)` returns a new array that keep all elements satisfy `p`.
*)

val keepWithIndexU: 'a t -> ('a -> int -> bool [@bs]) -> 'a t
val keepWithIndex: 'a t -> ('a -> int -> bool ) -> 'a t
(**
  `keepWithIndex(xs, p)`

  Returns a new array that keep all elements satisfy `p`.

  ```res example
  Belt.Array.keepWithIndex([1, 2, 3], (_x, i) => i == 1) == [2]
  ```
*)

val keepMapU: 'a t -> ('a -> 'b option [@bs]) -> 'b array
val keepMap: 'a t -> ('a -> 'b option) -> 'b array
(**
  `keepMap(xs, p)`

  Returns a new array that keep all elements that return a non-None applied `p`.

  ```res example
  Belt.Array.keepMap([1, 2, 3], x =>
    if mod(x, 2) == 0 {
      Some(x)
    } else {
      None
    }
  )
  == [2]
  ```
*)

val forEachWithIndexU: 'a t ->  (int -> 'a -> unit [@bs]) -> unit
val forEachWithIndex: 'a t ->  (int -> 'a -> unit ) -> unit
(**
  `forEachWithIndex(xs, f)`

  The same as `Belt.Array.forEach`;
  except that `f` is supplied two arguments: the index starting from 0 and the element from `xs`.

  ```res example
  Belt.Array.forEachWithIndex(["a", "b", "c"], (i, x) => Js.log("Item " ++ Belt.Int.toString(i) ++ " is " ++ x))

  /*
    prints:
    Item 0 is a
    Item 1 is b
    Item 2 is cc
  */
  let total = ref(0)

  Belt.Array.forEachWithIndex([10, 11, 12, 13], (i, x) => total := total.contents + x + i)

  total.contents == 0 + 10 + 1 + 11 + 2 + 12 + 3 + 13
  ```
*)

val mapWithIndexU: 'a t ->  (int -> 'a -> 'b [@bs]) -> 'b array
val mapWithIndex: 'a t ->  (int -> 'a -> 'b ) -> 'b array
(**
  `mapWithIndex(xs, f)`

  `mapWithIndex(xs, f)` applies `f` to each element of `xs`. Function `f` takes two arguments: the index starting from 0 and the element from `xs`.

  ```res example
  Belt.Array.mapWithIndex([1, 2, 3], (i, x) => i + x) == [0 + 1, 1 + 2, 2 + 3]
  ```
*)


val partitionU : 'a t -> ('a -> bool [@bs]) -> 'a t * 'a t
val partition : 'a t ->  ('a -> bool) -> 'a t * 'a t
(**
  `partition(f, a)` split array into tuple of two arrays based on predicate `f`; first of tuple where predicate cause true, second where predicate cause false

  ```res example
  Belt.Array.partition([1, 2, 3, 4, 5], (x) => mod(x, 2) == 0) == ([2, 4], [1, 3, 5])

  Belt.Array.partition([1, 2, 3, 4, 5], (x) => mod(x, 2) != 0) == ([1, 3, 5], [2, 4])
  ```
*)

val reduceU:  'b array -> 'a -> ('a -> 'b -> 'a [@bs]) ->'a
val reduce:  'b array -> 'a -> ('a -> 'b -> 'a ) ->'a
(**
  `reduce(xs, init, f)`

  Applies `f` to each element of `xs` from beginning to end. Function `f` has two parameters: the item from the list and an “accumulator”; which starts with a value of `init`. `reduce` returns the final value of the accumulator.

  ```res example
  Belt.Array.reduce([2, 3, 4], 1, (a, b) => a + b) == 10

  Belt.Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "abcd"
  ```
*)

val reduceReverseU: 'b array -> 'a -> ('a -> 'b ->  'a [@bs]) ->  'a
val reduceReverse: 'b array -> 'a -> ('a -> 'b ->  'a ) ->  'a
(**
  `reduceReverse(xs, init, f)`

  Works like `Belt_Array.reduce`; except that function `f` is applied to each item of `xs` from the last back to the first.

  ```res example
  Belt.Array.reduceReverse(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "dcba"
  ```
*)

val reduceReverse2U:
  'a t -> 'b array -> 'c  -> ('c -> 'a -> 'b ->  'c [@bs]) ->  'c
val reduceReverse2:
  'a t -> 'b array -> 'c  -> ('c -> 'a -> 'b ->  'c) ->  'c
(**
  `reduceReverse2(xs, ys, init, f)`

  Reduces two arrays xs and ys;taking items starting at `min(length(xs), length(ys))` down to and including zero.

  ```res example
  Belt.Array.reduceReverse2([1, 2, 3], [1, 2], 0, (acc, x, y) => acc + x + y) == 6
  ```
*)

val reduceWithIndexU:  'a t -> 'b -> ('b -> 'a -> int -> 'b [@bs]) -> 'b
val reduceWithIndex:  'a t -> 'b -> ('b -> 'a -> int -> 'b) -> 'b
(**
  Applies `f` to each element of `xs` from beginning to end. Function `f` has three parameters: the item from the array and an “accumulator”, which starts with a value of `init` and the index of each element. `reduceWithIndex` returns the final value of the accumulator.

  ```res example
  Belt.Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16
  ```
*)

val joinWithU: 'a t -> string -> ('a -> string [@bs]) -> string
val joinWith: 'a t -> string -> ('a -> string) -> string
(**
  `joinWith(xs, sep, toString)`

  Concatenates all the elements of `xs` converted to string with `toString`, each separated by `sep`, the string
  given as the second argument, into a single string.
  If the array has only one element, then that element will be returned
  without using the separator.
  If the array is empty, the empty string will be returned.

  ```res example
  joinWith([0, 1], ", ", string_of_int) == "0, 1"
  joinWith([], " ", string_of_int) == ""
  joinWith([1], " ", string_of_int) == "1"
  ```
*)

val someU: 'a t -> ('a -> bool [@bs]) -> bool
val some: 'a t -> ('a -> bool) -> bool
(**
  `some(xs, p)`

  Returns true if at least one of the elements in `xs` satifies `p`; where `p` is a predicate: a function taking an element and returning a `bool`.

  ```res example
  Belt.Array.some([2, 3, 4], (x) => mod(x, 2) == 1) == true

  Belt.Array.some([(-1), (-3), (-5)], (x) => x > 0) == false
  ```
*)

val everyU: 'a t -> ('a -> bool [@bs]) -> bool
val every: 'a t -> ('a -> bool ) -> bool
(**
  `every(xs, p)`

  Returns `true` if all elements satisfy `p`; where `p` is a predicate: a function taking an element and returning a `bool`.

  ```res example
  Belt.Array.every([1, 3, 5], (x) => mod(x, 2) == 1) == true

  Belt.Array.every([1, (-3), 5], (x) => x > 0) == false
  ```
*)

val every2U: 'a t -> 'b array -> ('a -> 'b -> bool [@bs]) -> bool
val every2: 'a t -> 'b array -> ('a -> 'b -> bool ) -> bool
(**
  `every2(xs, ys, p)`

  returns true if `p(xi, yi)` is true for all pairs of elements up to the shorter length (i.e. `min(length(xs), length(ys))`)

  ```res example
  Belt.Array.every2([1, 2, 3], [0, 1], (a, b) => a > b) == true

  Belt.Array.every2([], [1], (x, y) => x > y) == true

  Belt.Array.every2([2, 3], [1], (x, y) => x > y) == true

  Belt.Array.every2([0, 1], [5, 0], (x, y) => x > y) == false
  ```
*)

val some2U: 'a t -> 'b array -> ('a -> 'b -> bool [@bs]) -> bool
val some2: 'a t -> 'b array -> ('a -> 'b -> bool ) -> bool
(**
  `some2(xs, ys, p)`

  returns true if `p(xi, yi)` is true for any pair of elements up to the shorter length (i.e. `min(length(xs), length(ys))`)

  ```res example
  Belt.Array.some2([0, 2], [1, 0, 3], (a, b) => a > b) == true

  Belt.Array.some2([], [1], (x, y) => x > y) == false

  Belt.Array.some2([2, 3], [1, 4], (x, y) => x > y) == true
  ```
*)

val cmpU: 'a t -> 'a t -> ('a -> 'a -> int [@bs]) -> int
val cmp: 'a t -> 'a t -> ('a -> 'a -> int ) -> int
(**
  `cmp(xs, ys, f)`

  Compared by length if `length(xs) != length(ys)`; returning -1 if `length(xs) < length(ys)` or 1 if `length(xs) > length(ys)`
  Otherwise compare one by one `f(x, y)`. `f` returns
  a negative number if `x` is “less than” `y`
  zero if `x` is “equal to” `y`
  a positive number if `x` is “greater than” `y`
  The comparison returns the first non-zero result of `f`;or zero if `f` returns zero for all `x` and `y`.

  ```res example
  Belt.Array.cmp([1, 3, 5], [1, 4, 2], (a, b) => compare(a, b)) == -1

  Belt.Array.cmp([1, 3, 5], [1, 2, 3], (a, b) => compare(a, b)) == 1

  Belt.Array.cmp([1, 3, 5], [1, 3, 5], (a, b) => compare(a, b)) == 0
  ```
*)

val eqU:  'a t -> 'a t -> ('a -> 'a -> bool [@bs]) -> bool
val eq:  'a t -> 'a t -> ('a -> 'a -> bool ) -> bool
(**
  `eq(xs, ys)`

  return false if length is not the same
  otherwise compare items one by one using `f(xi, yi)`; and return true if all results are truefalse otherwise

  ```res example
  Belt.Array.eq([1, 2, 3], [(-1), (-2), (-3)], (a, b) => abs(a) == abs(b)) == true
  ```
*)

external truncateToLengthUnsafe: 'a t -> int ->  unit = "length" [@@bs.set]
(**
  Unsafe `truncateToLengthUnsafe(xs, n)` sets length of array `xs` to `n`.

  If `n` is greater than the length of `xs`; the extra elements are set to `Js.Null_undefined.null`.

  If `n` is less than zero; raises a `RangeError`.

  ```res example
  let arr = ["ant", "bee", "cat", "dog", "elk"]

  Belt.Array.truncateToLengthUnsafe(arr, 3)

  arr == ["ant", "bee", "cat"]
  ```
*)


val initU : int -> (int -> 'a [@bs]) -> 'a t 
val init : int -> (int -> 'a) -> 'a t 

(**
  `arr->push(item)` pushes an element `item` into an array `arr`.
*)
external push : 'a t -> 'a -> unit = "push" [@@send]
