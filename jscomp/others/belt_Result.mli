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

(** [`Belt.Result`]()

    Utilities for result data type.
*)


(**
  `Belt.Result` is a data type with two variants: `Ok` and `Error`. Each of these variants can
  contain data, and those two pieces of data need not have the same data type. `Belt.Result` is
  useful when you need to not only determine whether some data is valid or not (use `Belt.Option`
  for that), but also keep information about the invalid data.

  In the examples, we presume the existence of two variables:

  ```
  let good = Ok 42
  let bad = Error "Invalid data"
  ```
*)

type ('a,'b) t = Ok of 'a | Error of 'b

val getExn : ('a, 'b) t -> 'a
(**
  `getExn res`

  when `res` is `Ok n`, returns `n`
  when `res` is `Error m`, **raise** an exception

  ```
  getExn good = 42;;
  getExn bad;; (* raises exception *)
  ```
*)

val mapWithDefaultU : ('a, 'c) t -> 'b -> ('a -> 'b [@bs]) -> 'b
val mapWithDefault : ('a, 'c) t -> 'b -> ('a -> 'b) -> 'b
(**
  `mapWithDefault res default f`

  When `res` is `Ok n`, returns `f n`, otherwise `default`.

  ```
  mapWithDefault good 0 (fun x -> x / 2) = 21
  mapWithDefault bad 0 (fun x -> x / 2) = 0
  ```
*)

val mapU : ('a, 'c) t -> ('a -> 'b [@bs]) -> ('b, 'c) t
val map : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
(**
  `map res f`

  When `res` is `Ok n`, returns `Ok (f n)`. Otherwise returns `res` unchanged.
  Function `f` takes a value of the same type as `n` and returns an ordinary value.

  ```
  let f x = sqrt (float_of_int x)
  map (Ok 64) f = Ok 8.0
  map (Error "Invalid data") f = Error "Invalid data"
  ```
*)

val flatMapU : ('a, 'c) t -> ('a -> ('b, 'c) t [@bs]) -> ('b, 'c) t
val flatMap : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
(**
  `flatMap res f`

  When `res` is `Ok n`, returns `f n`. Otherwise, returns `res` unchanged.
  Function `f` takes a value of the same type as `n` and returns a `Belt.Result`.

  ```
  let recip x =
    if x != 0.0
    then
      Ok (1.0 /. x)
    else
      Error "Divide by zero"

  flatMap (Ok 2.0) recip = Ok 0.5
  flatMap (Ok 0.0) recip = Error "Divide by zero"
  flatMap (Error "Already bad") recip = Error "Already bad"
  ```
*)

val getWithDefault : ('a, 'b) t -> 'a -> 'a
(**
  `getWithDefault res defaultValue`

  if `res` is `Ok n`, returns `n`, otherwise `default`

  ```
  getWithDefault (Ok 42) 0 = 42
  getWithDefault (Error "Invalid Data") = 0
  ```
*)

val isOk : ('a, 'b) t -> bool
(**
   `isOk res`

   Returns `true` if `res` is of the form `Ok n`, `false` if it is the `Error e` variant.
*)

val isError : ('a, 'b) t -> bool
(**
   `isError res`

   Returns `true` if `res` is of the form `Error e`, `false` if it is the `Ok n` variant.
*)

val eqU : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> bool [@bs]) -> bool
val eq : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> bool) -> bool
(**
  `eq res1 res2 f`

  Determine if two `Belt.Result` variables are equal with respect to an equality function.
  If `res1` and `res2` are of the form `Ok n` and `Ok m`, return the result of `f n m`.
  If one of `res1` and `res2` are of the form `Error e`, return false
  If both `res1` and `res2` are of the form `Error e`, return true

  ```
  let good1 = Ok 42
  let good2 = Ok 32
  let bad1 = Error "invalid"
  let bad2 = Error "really invalid"

  let mod10equal a b =
    a mod 10 == b mod 10

  eq good1 good2 mod10equal = true
  eq good1 bad1 mod10equal = false
  eq bad2 good2 mod10equal = false
  eq bad1 bad2 mod10equal = true
  ```
*)

val cmpU : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> int [@bs]) -> int
val cmp : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> int) -> int
(**
  `cmp res1 res2 f`

  Compare two `Belt.Result` variables with respect to a comparison function.
  The comparison function returns -1 if the first variable is "less than" the second,
  0 if the two variables are equal, and 1 if the first is "greater than" the second.

  If `res1` and `res2` are of the form `Ok n` and `Ok m`, return the result of `f n m`.
  If `res1` is of the form `Error e` and `res2` of the form `Ok n`, return -1 (nothing is less than something)
  If `res1` is of the form `Ok n` and `res2` of the form `Error e`, return 1 (something is greater than nothing)
  If both `res1` and `res2` are of the form `Error e`, return 0 (equal)

  ```
  let good1 = Ok 59
  let good2 = Ok 37
  let bad1 = Error "invalid"
  let bad2 = Error "really invalid"

  let mod10cmp a b =
    Pervasives.compare (a mod 10) (b mod 10)

  cmp (Ok 39) (Ok 57) mod10cmp = 1
  cmp (Ok 57) (Ok 39) mod10cmp = -1
  cmp (Ok 39) (Error "y") mod10cmp = 1
  cmp (Error "x") (Ok 57) mod10cmp = -1
  cmp (Error "x") (Error "y") mod10cmp = 0
  ```
*)
