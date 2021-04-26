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


(** [`Belt.Array`]()
  Utililites for Array functions
*)


external length: 'a array -> int = "%array_length"
(** `length xs` return the size of the array *)

external size: 'a array -> int = "%array_length"
(** **See** [`length`]() *)

val get: 'a array -> int -> 'a option
(**
  `get arr i`

  If `i <= 0 <= length arr`, returns `Some value` where `value` is the item at index `i`
  If `i` is out of range, returns `None`

  ```
  Belt.Array.get [|"a";"b";"c"|] 0 = Some "a";;
  Belt.Array.get [|"a";"b";"c"|] 3 = None;;
  Belt.Array.get [|"a";"b";"c"|] (-1) = None;;
  ```
*)

val getExn: 'a array -> int -> 'a
(**
  `getExn arr i`

  **raise** an exception if `i` is out of range;otherwise return the value at index `i` in `arr`
*)

external getUnsafe: 'a array -> int -> 'a = "%array_unsafe_get"
(**
  `getUnsafe arr i`

  **Unsafe**

  no bounds checking; this would cause type error
  if `i` does not stay within range
*)

external getUndefined: 'a array -> int -> 'a Js.undefined = "%array_unsafe_get"
(**
  `getUndefined arr i`

  It does the samething in the runtime as [`getUnsafe`]();
  it is _type safe_ since the return type still track whether it is
  in range or not
*)

val set: 'a array -> int -> 'a -> bool
(**
  `set arr n x` modifies `arr` in place;
  it replaces the nth element of `arr` with `x`

  **return** false means not updated due to out of range
*)

val setExn: 'a array -> int -> 'a -> unit
(**
  `setExn arr i x`

  **raise** an exception if `i` is out of range
*)

external setUnsafe: 'a array -> int -> 'a -> unit = "%array_unsafe_set"

val shuffleInPlace: 'a array -> unit
(** `shuffleInPlace arr` randomly re-orders the items in `arr` *)

val shuffle: 'a array -> 'a array
(** `shuffle xs`
  **return** a fresh array with items in original array randomly shuffled *)

val reverseInPlace: 'a array -> unit
(**
  `reverseInPlace arr` reverses items in `arr` in place

  ```
  let arr = [|10;11;12;13;14|];;
  let () = reverseInPlace arr;;
  arr = [|14;13;12;11;10|];;
  ```
*)

val reverse: 'a array -> 'a array
(**
  `reverse arr`

  **return** a fresh array with items in `arr` in reverse order

  ```
  reverse [|10;11;12;13;14|] = [|14;13;12;11;10|];;
  ```
*)

external makeUninitialized: int -> 'a Js.undefined array = "Array" [@@bs.new]
(**
   `makeUninitialized n` creates an array of length `n` filled with the undefined value.
   You must specify the type of data that will eventually fill the array.

   ```
   let arr: string Js.undefined array = makeUninitialized 5;;
   getExn arr 0 = Js.undefined;;
   ```
*)

external makeUninitializedUnsafe: int -> 'a array = "Array" [@@bs.new]
(**
  `makeUninitializedUnsafe n`

  **Unsafe**

  ```
  let arr = Belt.Array.makeUninitializedUnsafe 5;;
  let () = Js.log(Belt.Array.getExn arr 0);; (* undefined *)
  Belt.Array.setExn arr 0 "example";;
  let () = Js.log(Belt.Array.getExn arr 0 = "example");;
  ```
*)


val make: int -> 'a  -> 'a array
(**
  `make n e`

  **return** an array of size `n` filled  with value `e`

  **return** an empty array when `n` is negative.
*)

val range: int -> int -> int array
(**
  `range start finish` create an inclusive array

  ```
  range 0 3 =  [|0;1;2;3|];;
  range 3 0 =  [||] ;;
  range 3 3 = [|3|];;
  ```
*)

val rangeBy: int -> int -> step:int -> int array
(**
  `rangeBy start finish ~step`

  **return** empty array when step is 0 or negative
  it also return empty array when `start > finish`

  ```
  rangeBy 0 10 ~step:3 = [|0;3;6;9|];;
  rangeBy 0 12 ~step:3 = [|0;3;6;9;12|];;
  rangeBy 33 0 ~step:1 =  [||];;
  rangeBy 33 0 ~step:(-1) = [||];;
  rangeBy 3 12 ~step:(-1) = [||];;
  rangeBy 3 3 ~step:0 = [||] ;;
  rangeBy 3 3 ~step:(1) = [|3|] ;;
  ```
*)

val makeByU: int -> (int -> 'a [@bs]) -> 'a array
val makeBy: int -> (int -> 'a ) -> 'a array
(**
  `makeBy n f`

  **return** an empty array when `n` is negative

  **return** an array of size `n` populated by `f i` start from `0` to `n - 1`

  ```
  makeBy 5 (fun i -> i) = [|0;1;2;3;4|];;
  makeBy 5 (fun i -> i * i) = [|0;1;4;9;16|]
  ```
*)

val makeByAndShuffleU: int -> (int -> 'a [@bs]) -> 'a array
val makeByAndShuffle: int -> (int -> 'a ) -> 'a array
(**
  `makeByAndShuffle n f`
  Equivalent to `shuffle (makeBy n f)`
*)


val zip: 'a array -> 'b array -> ('a * 'b) array
(**
  `zip a b`

  Create an array of pairs from corresponding elements of `a` and `b`.
  Stop with the shorter array

  ```
  zip [|1;2|] [|3;4;5|] = [|(1, 3);(2, 4)|]
  ```
*)


val zipByU: 'a array -> 'b array -> ('a -> 'b -> 'c [@bs]) -> 'c array
val zipBy: 'a array -> 'b array -> ('a -> 'b -> 'c ) -> 'c array
(**
   `zipBy xs ys f`

   Create an array by applying `f` to corresponding elements of `xs` and `ys`
   Stops with shorter array

   Equivalent to `map (zip xs ys) (fun (a,b) -> f a b) `

   ```
   zipBy [|1;2;3|] [|4;5|] (fun a b -> 2 * a + b) = [|6;9|];;
   ```
*)

val unzip: ('a * 'b) array -> 'a array * 'b array
(**
  `unzip a` takes an array of pairs and creates a pair of arrays. The first array contains all the first items of the pairs; the second array contains all the second items.

  ```
  unzip [|(1,2) ; (3,4)|] = ([|1;3|], [|2;4|]);;
  unzip [|(1,2) ; (3,4) ; (5,6) ; (7,8)|] = ([|1;3;5;7|], [|2;4;6;8|]);;
  ```
*)

val concat: 'a array -> 'a array -> 'a array
(**
  `concat xs ys`

  **return** a fresh array containing the
  concatenation of the arrays `v1` and `v2`; so even if `v1` or `v2`
  is empty;it can not be shared

  ```
  concat [|1;2;3|] [|4;5|] = [|1;2;3;4;5|];;
  concat [| |] [|"a";"b";"c"|] = [|"a";"b";"c"|];;
  ```
*)

val concatMany: 'a array array -> 'a array
(**
  `concatMany xss`

  **return** a fresh array as the concatenation of `xss` (an array of arrays)

  ```
  concatMany [| [|1;2;3|]; [|4;5;6|]; [|7;8|] |] = [|1;2;3;4;5;6;7;8|];;
  ```
*)

val slice: 'a array -> offset:int -> len:int -> 'a array
(**
  `slice xs offset len` creates a new array with the `len` elements of `xs` starting at `offset` for

  `offset` can be negative and is evaluated as `length xs - offset`
  `slice xs -1 1` means get the last element as a singleton array

  `slice xs (-len) len` will return a copy of the array

  if the array does not have enough data;`slice` extracts through
  the end of sequence.

  if `len` is negative;returns the empty array.

  ```
  slice [|10;11;12;13;14;15;16|] ~offset: 2 ~len: 3 = [|12;13;14|];;
  slice [|10;11;12;13;14;15;16|] ~offset: (-4) ~len: 3 = [|13;14;15|];;
  slice [|10;11;12;13;14;15;16|] ~offset:4  ~len:9 = [|14;15;16|];;
  ```
*)

val sliceToEnd: 'a array -> int -> 'a array
(**
  `sliceToEnd xs offset` creates a new array with the elements of `xs` starting at `offset`

  `offset` can be negative and is evaluated as `length xs - offset`
  `sliceToEnd xs -1` means get the last element as a singleton array

  `sliceToEnd xs 0` will return a copy of the array

  ```
  sliceToEnd [|10;11;12;13;14;15;16|] 2 = [|12;13;14;15;16|];;
  sliceToEnd [|10;11;12;13;14;15;16|] (-4) = [|13;14;15;16|];;
  ```
*)


external copy : 'a array -> (_ [@bs.as 0]) -> 'a array = "slice" [@@bs.send]
(**
  `copy a`

  **return** a shallow copy of `a`
*)

val fill: 'a array -> offset:int -> len:int -> 'a -> unit
(**
  `fill arr ~offset ~len x`

  Modifies `arr` in place,
  storing `x` in elements number `offset` to `offset + len - 1`.

  `offset` can be negative and is evaluated as `length arr - offset`

  `fill arr ~offset:(-1) ~len:1` means fill the last element,
  if the array does not have enough data;`fill` will ignore it

  ```
  let arr = makeBy 5 (fun i -> i) ;;
  fill arr ~offset:2 ~len:2 9 ;;
  arr = [|0;1;9;9;4|];;
  fill arr ~offset:7 ~len:2 8;;
  arr = [|0;1;9;9;4|];;
  ```
*)

val blit:
  src:'a array -> srcOffset:int -> dst:'a array -> dstOffset:int -> len:int -> unit
(**
  `blit ~src:v1 ~srcOffset:o1 ~dst:v2 ~dstOffset:o2 ~len`

  copies `len` elements
  from array `v1`, starting at element number `o1` to array `v2`,
  starting at element number `o2`.

  It works correctly even if
  `v1` and `v2` are the same array;and the source and
  destination chunks overlap.

  `offset` can be negative. `-1` means `len - 1`. If `len + offset` is still
  negative, it will be set as 0

  For each of the examples;presume that `v1 = [|10;11;12;13;14;15;16;17|]` and
  `v2 = [|20;21;22;23;24;25;26;27|]`. The result shown is the content of the destination array.

  ```
  Belt.Array.blit [|20;21;14;15;16;25;26;27|]
    ~src: v1 ~srcOffset: 4 ~dst: v2 ~dstOffset: 2 ~len: 3

  Belt.Array.blit [|10;11;14;15;16;15;16;17|]
    ~src: v1 ~srcOffset: 4 ~dst: v1 ~dstOffset: 2 ~len: 3
  ```
*)

val blitUnsafe:
  src:'a array -> srcOffset:int -> dst:'a array -> dstOffset:int -> len:int -> unit
(**
  **Unsafe** blit without bounds checking
*)

val forEachU: 'a array ->  ('a -> unit [@bs]) -> unit
val forEach: 'a array ->  ('a -> unit ) -> unit
(**
  `forEach xs f`

  Call `f` on each element of `xs` from the beginning to end. `f` returns `unit`;so no
  new array is created. Use `forEach` when you are primarily concerned with repetitively
  creating side effects.

  ```
  forEach [|"a";"b";"c"|] (fun x -> Js.log("Item: " ^ x));;
  (*  prints:
    Item: a
    Item: b
    Item: c
  *)

  let total = ref 0;;
  forEach [|1;2;3;4|] (fun x -> total := !total + x);;
  !total  = 1 + 2 + 3 + 4;;
  ```
*)

val mapU: 'a array ->  ('a -> 'b [@bs]) -> 'b array
val map: 'a array ->  ('a -> 'b ) -> 'b array
(**
  `map xs f `

  **return** a new array by calling `f` for each element of `xs` from
  the beginning to end

  ```
  map [|1;2|] (fun x-> x + 10) = [|11;12|]
  ```
*)

val getByU: 'a array -> ('a -> bool [@bs]) -> 'a option
val getBy: 'a array -> ('a -> bool) -> 'a option
(**
  `getBy xs p` returns `Some value` for the first value in `xs` that satisifies the predicate function `p`; returns `None` if no element satisifies the function.

  ```
  getBy [|1;4;3;2|] (fun x -> x mod 2 = 0) = Some 4
  getBy [|15;13;11|] (fun x -> x mod 2 = 0) = None
  ```
*)

val getIndexByU: 'a array -> ('a -> bool [@bs]) -> int option
val getIndexBy: 'a array -> ('a -> bool) -> int option
(**
  `getIndexBy xs p` returns `Some index` for the first value in `xs` that satisifies the predicate function `p`; returns `None` if no element satisifies the function.

  ```
  getIndexBy [|1;4;3;2|] (fun x -> x mod 2 = 0) = Some 1
  getIndexBy [|15;13;11|] (fun x -> x mod 2 = 0) = None
  ```
*)

val keepU: 'a array -> ('a -> bool [@bs]) -> 'a array
val keep: 'a array -> ('a -> bool ) -> 'a array
(**
  `keep xs p `

  **return** a new array that keeps all elements satisfying `p`

  ```
  keep [|1;2;3|] (fun x -> x mod  2 = 0) = [|2|]
  ```
*)

val keepWithIndexU: 'a array -> ('a -> int -> bool [@bs]) -> 'a array
val keepWithIndex: 'a array -> ('a -> int -> bool ) -> 'a array
(**
  `keepWithIndex xs p `

  **return** a new array that keeps all elements satisfying `p`.

  The predicate `p` takes two arguments:
  the element from `xs` and the index starting from 0.

  ```
  keepWithIndex [|1;2;3|] (fun _x i -> i = 1) = [|2|]
  ```
*)

val keepMapU: 'a array -> ('a -> 'b option [@bs]) -> 'b array
val keepMap: 'a array -> ('a -> 'b option) -> 'b array
(**
  `keepMap xs p`

  **return** a new array that keeps all elements that return a non-None when applied to `p`

  ```
  keepMap [|1;2;3|] (fun x -> if x mod 2 = 0 then Some x else None)
    = [| 2 |]
  ```
*)

val forEachWithIndexU: 'a array ->  (int -> 'a -> unit [@bs]) -> unit
val forEachWithIndex: 'a array ->  (int -> 'a -> unit ) -> unit
(**
  `forEachWithIndex xs f`

  The same as [`forEach`](); except that `f` is supplied with two arguments:
  the index starting from 0 and the element from `xs`

  ```
  forEachWithIndex [|"a";"b";"c"|] (fun i x -> Js.log("Item " ^ (string_of_int i) ^ " is " ^ x));;
  (* prints:
    Item 0 is a
    Item 1 is b
    Item 2 is c
  *)

  let total = ref 0 ;;
  forEachWithIndex [|10;11;12;13|] (fun i x -> total := !total + x + i);;
  !total = 0 + 10 + 1 +  11 + 2 + 12 + 3 + 13;;
  ```
*)

val mapWithIndexU: 'a array ->  (int -> 'a -> 'b [@bs]) -> 'b array
val mapWithIndex: 'a array ->  (int -> 'a -> 'b ) -> 'b array
(**
  `mapWithIndex xs f `

  `mapWithIndex xs f` applies `f` to each element of `xs`. Function `f` takes two arguments:
  the index starting from 0 and the element from `xs`.

  ```
  mapWithIndex [|1;2;3|] (fun i x -> i + x) =
    [|0 + 1; 1 + 2; 2 + 3|]
  ```
*)


val partitionU : 'a array -> ('a -> bool [@bs]) -> 'a array * 'a array
val partition : 'a array ->  ('a -> bool) -> 'a array * 'a array
(**
  `partition f a` split array into tuple of two arrays based on predicate f; first of tuple where predicate cause true, second where predicate cause false

  ```
  partition [|1;2;3;4;5|] (fun x -> x mod 2 = 0  ) = ([|2;4|], [|1;2;3|]);;
  partition [|1;2;3;4;5|] (fun x -> x mod 2 <> 0 ) = ([|1;2;3|], [|2;4|]);;
  ```
*)

val reduceU:  'b array -> 'a -> ('a -> 'b -> 'a [@bs]) ->'a
val reduce:  'b array -> 'a -> ('a -> 'b -> 'a ) ->'a
(**
  `reduce xs init f`

  Applies `f` to each element of `xs` from beginning to end.  Function `f` has two parameters: the item
  from the list and an “accumulator”;which starts with a value of `init`. `reduce`
  returns the final value of the accumulator.

  ```
  reduce [|2;3;4|] 1 (+) = 10;;
  reduce [|"a";"b";"c";"d"|] "" (^) = "abcd";;
  ```
*)

val reduceReverseU: 'b array -> 'a -> ('a -> 'b ->  'a [@bs]) ->  'a
val reduceReverse: 'b array -> 'a -> ('a -> 'b ->  'a ) ->  'a
(**
  `reduceReverse xs init f`

  Works like [`reduce`]();except that function `f` is applied to each item of `xs` from the last
  back to the first.

  ```
  reduceReverse [|"a";"b";"c";"d"|] "" (^) = "dcba";;
  ```
*)

val reduceReverse2U:
  'a array -> 'b array -> 'c  -> ('c -> 'a -> 'b ->  'c [@bs]) ->  'c
val reduceReverse2:
  'a array -> 'b array -> 'c  -> ('c -> 'a -> 'b ->  'c) ->  'c
(**
  `reduceReverse2 xs ys init f`
  Reduces two arrays `xs` and `ys`, taking items starting at `min (length xs) (length ys)`
  down to and including zero.

  ```
  reduceReverse2 [|1;2;3|] [|1;2|] 0 (fun acc x y -> acc + x + y) = 6
  ```
*)

val reduceWithIndexU:  'a array -> 'b -> ('b -> 'a -> int -> 'b [@bs]) -> 'b
val reduceWithIndex:  'a array -> 'b -> ('b -> 'a -> int -> 'b) -> 'b
(**
  `reduceWithIndex xs f`

  Applies `f` to each element of `xs` from beginning to end. Function `f` has three parameters: the item
  from the array and an “accumulator”, which starts with a value of `init` and the index of each element. `reduceWithIndex`
  returns the final value of the accumulator.

  ```
  reduceWithIndex [|1;2;3;4|] 0 (fun acc x i -> acc + x + i) = 16;
  ```
*)

val joinWithU: 'a array -> string -> ('a -> string [@bs]) -> string
val joinWith: 'a array -> string -> ('a -> string) -> string
(**
  `joinWith xs sep toString`

  Concatenates all the elements of `xs` converted to string with `toString`, each separated by `sep`, the string
  given as the second argument, into a single string.
  If the array has only one element, then that element will be returned
  without using the separator.
  If the array is empty, the empty string will be returned.

  ```
  joinWith [|0; 1|] ", " string_of_int = "0, 1"
  joinWith [||] " " string_of_int = ""
  joinWith [|1|] " " string_of_int = "1"
  ```
*)

val someU: 'a array -> ('a -> bool [@bs]) -> bool
val some: 'a array -> ('a -> bool) -> bool
(**
  `some xs p`

  **return** `true` if at least one of the elements in `xs` satifies `p`, where `p` is a _predicate_: a function taking
  an element and returning a `bool`.

  ```
  some [|2; 3; 4|] (fun x -> x mod 2 = 1) = true;;
  some [|-1; -3; -5|] (fun x -> x > 0) = false;;
  ```
*)

val everyU: 'a array -> ('a -> bool [@bs]) -> bool
val every: 'a array -> ('a -> bool ) -> bool
(**
  `every xs p`

  **return** true if all elements satisfy `p`; where `p` is a _predicate_: a function taking
  an element and returning a `bool`.

  ```
  every [|1; 3; 5|] (fun x -> x mod 2 = 1) = true;;
  every [|1; -3; 5|] (fun x -> x > 0) = false;;
  ```
*)

val every2U: 'a array -> 'b array -> ('a -> 'b -> bool [@bs]) -> bool
val every2: 'a array -> 'b array -> ('a -> 'b -> bool ) -> bool
(**
  `every2 xs ys p` returns true if `p xi yi` is true for all pairs of elements
  up to the shorter length (i.e. `min (length xs) (length ys)`)

  ```
  every2 [|1;2;3|] [|0;1|] (>) = true;;
  every2 [||] [|1|] (fun  x y -> x > y) = true;;
  every2 [|2;3|] [|1|] (fun  x y -> x > y) = true;;
  every2 [|0;1|] [|5;0|] (fun x y -> x > y) = false;
  ```
*)

val some2U: 'a array -> 'b array -> ('a -> 'b -> bool [@bs]) -> bool
val some2: 'a array -> 'b array -> ('a -> 'b -> bool ) -> bool
(**
  `some2 xs ys p` returns true if `p xi yi` is true for any pair of elements
  up to the shorter length (i.e. `min (length xs) (length ys)`)

  ```
  some2 [|0;2|] [|1;0;3|] (>) = true ;;
  (some2 [||] [|1|] (fun   x y -> x > y)) =  false;;
  (some2 [|2;3|] [|1;4|] (fun   x y -> x > y)) = true;;
  ```
*)

val cmpU: 'a array -> 'a array -> ('a -> 'a -> int [@bs]) -> int
val cmp: 'a array -> 'a array -> ('a -> 'a -> int ) -> int
(**
  `cmp xs ys f`

  - Compared by length if `length xs <> length ys`;returning -1 if`length xs < length ys` or 1 if `length xs > length ys`
  - Otherwise compare one by one `f x y`. `f` returns
  - a negative number if `x` is “less than” `y`
  - zero if `x` is “equal to” `y`
  - a positive number if `x` is “greater than” `y`
  - The comparison returns the first non-zero result of `f`; or zero if `f` returns zero for all `x` and `y`.

  ```
  cmp [|1; 3; 5|] [|1; 4; 2|] (fun a b -> compare a b) = -1;;
  cmp [|1; 3; 5|] [|1; 2; 3|] (fun a b -> compare a b) = 1;;
  cmp [|1; 3; 5|] [|1; 3; 5|] (fun a b -> compare a b) = 0;;
  ```
*)

val eqU:  'a array -> 'a array -> ('a -> 'a -> bool [@bs]) -> bool
val eq:  'a array -> 'a array -> ('a -> 'a -> bool ) -> bool
(**
  `eq xs ys`

  - return false if length is not the same
  - otherwise compare items one by one using `f xi yi`;and return true if all results are true;false otherwise

  ```
  eq [|1; 2; 3|] [|-1; -2; -3|] (fun a b -> abs a = abs b) = true
  ```
*)

external truncateToLengthUnsafe: 'a array -> int ->  unit = "length" [@@bs.set]
(**
  **Unsafe**

  `truncateToLengthUnsafe xs n` sets length of array `xs` to `n`.

  If `n` is greater than the length of `xs`; the extra elements are set to `Js.Null_undefined.null`

  If `n` is less than zero;raises a `RangeError`.

  ```
  let arr = [|"ant";"bee";"cat";"dog";"elk"|];;
  let () = truncateToLengthUnsafe arr 3;;
  arr = [|"ant";"bee";"cat"|];;
  ```
*)
