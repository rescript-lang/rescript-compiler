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
  Result types are really useful to describe the result of a certain operation
  without relying on exceptions or `option` types.

  This module gives you useful utilities to create and combine `Result` data.
*)

type ('a,'b) t = Ok of 'a | Error of 'b
(**
  The type `Result.t(result, err)` describes a variant of two states:
  `Ok(someResult)` represents a successful operation, whereby
  ``Error(someError)` signals an erronous operation.

  In this concrete example, we are defining our own `Result` type to reflect an HTTP like
  query operation:

  ```res example
  type responseError = NotAvailable | NotFound
  type queryResult = t<string, responseError>

  let failQueryUser = (username: string): queryResult => {
    Error(NotAvailable)
  }
```
*)

val getExn : ('a, 'b) t -> 'a
(**
  `getExn(res)`: when `res` is `Ok(n)`, returns `n` when `res` is `Error(m)`, raise an exception

  ```res example
  Belt.Result.getExn(Belt.Result.Ok(42)) == 42

  Belt.Result.getExn(Belt.Result.Error("Invalid data")) /* raises exception */
  ```
*)

val mapWithDefaultU : ('a, 'c) t -> 'b -> ('a -> 'b [@bs]) -> 'b
val mapWithDefault : ('a, 'c) t -> 'b -> ('a -> 'b) -> 'b
(**
  `mapWithDefault(res, default, f)`: When res is `Ok(n)`, returns `f(n)`,
  otherwise `default`.

  ```res example
  let ok = Belt.Result.Ok(42)
  Belt.Result.mapWithDefault(ok, 0, (x) => x / 2) == 21

  let error = Belt.Result.Error("Invalid data")
  Belt.Result.mapWithDefault(error, 0, (x) => x / 2) == 0
  ```
*)

val mapU : ('a, 'c) t -> ('a -> 'b [@bs]) -> ('b, 'c) t
val map : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
(**
  `map(res, f)`: When res is `Ok(n)`, returns `Ok(f(n))`. Otherwise returns res
  unchanged. Function `f` takes a value of the same type as `n` and returns an
  ordinary value.

  ```res example
  let f = (x) => sqrt(Belt.Int.toFloat(x))

  Belt.Result.map(Ok(64), f) == Ok(8.0)

  Belt.Result.map(Error("Invalid data"), f) == Error("Invalid data")
  ```
*)

val flatMapU : ('a, 'c) t -> ('a -> ('b, 'c) t [@bs]) -> ('b, 'c) t
val flatMap : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
(**
  `flatMap(res, f)`: When res is `Ok(n)`, returns `f(n)`. Otherwise, returns res
  unchanged. Function `f` takes a value of the same type as `n` and returns a
  `Belt.Result`.

  ```res example
  let recip = (x) =>
    if (x !== 0.0) {
      Belt.Result.Ok(1.0 /. x)
    } else {
      Belt.Result.Error("Divide by zero")
    }

  Belt.Result.flatMap(Ok(2.0), recip) == Ok(0.5)

  Belt.Result.flatMap(Ok(0.0), recip) == Error("Divide by zero")

  Belt.Result.flatMap(Error("Already bad"), recip) == Error("Already bad")
  ```
*)

val getWithDefault : ('a, 'b) t -> 'a -> 'a
(**
  `getWithDefault(res, defaultValue)`: If `res` is `Ok(n)`, returns `n`,
  otherwise `default`

  ```res example
  Belt.Result.getWithDefault(Ok(42), 0) == 42

  Belt.Result.getWithDefault(Error("Invalid Data"), 0) == 0
  ```
*)

val isOk : ('a, 'b) t -> bool
(**
  `isOk(res)`: Returns `true` if `res` is of the form `Ok(n)`, `false` if it is
  the `Error(e)` variant.
*)

val isError : ('a, 'b) t -> bool
(**
  `isError(res)`: Returns `true` if `res` is of the form `Error(e)`, `false` if
  it is the `Ok(n)` variant.
*)

val eqU : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> bool [@bs]) -> bool
val eq : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> bool) -> bool
(**
  `eq(res1, res2, f)`: Determine if two `Belt.Result` variables are equal with
  respect to an equality function. If `res1` and `res2` are of the form `Ok(n)`
  and `Ok(m)`, return the result of `f(n, m)`. If one of `res1` and `res2` are of
  the form `Error(e)`, return false If both `res1` and `res2` are of the form
  `Error(e)`, return true

  ```res example
  let good1 = Belt.Result.Ok(42)

  let good2 = Belt.Result.Ok(32)

  let bad1 = Belt.Result.Error("invalid")

  let bad2 = Belt.Result.Error("really invalid")

  let mod10equal = (a, b) => mod(a, 10) === mod(b, 10)

  Belt.Result.eq(good1, good2, mod10equal) == true

  Belt.Result.eq(good1, bad1, mod10equal) == false

  Belt.Result.eq(bad2, good2, mod10equal) == false

  Belt.Result.eq(bad1, bad2, mod10equal) == true
  ```
*)

val cmpU : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> int [@bs]) -> int
val cmp : ('a, 'c) t -> ('b, 'd) t -> ('a -> 'b -> int) -> int
(**
  `cmp(res1, res2, f)`: Compare two `Belt.Result` variables with respect to a
  comparison function. The comparison function returns -1 if the first variable
  is "less than" the second, 0 if the two variables are equal, and 1 if the first
  is "greater than" the second.

  If `res1` and `res2` are of the form `Ok(n)` and `Ok(m)`, return the result of
  `f(n, m)`. If `res1` is of the form `Error(e)` and `res2` of the form `Ok(n)`,
  return -1 (nothing is less than something) If `res1` is of the form `Ok(n)` and
  `res2` of the form `Error(e)`, return 1 (something is greater than nothing) If
  both `res1` and `res2` are of the form `Error(e)`, return 0 (equal)

  ```res example
  let good1 = Belt.Result.Ok(59)

  let good2 = Belt.Result.Ok(37)

  let bad1 = Belt.Result.Error("invalid")

  let bad2 = Belt.Result.Error("really invalid")

  let mod10cmp = (a, b) => Pervasives.compare(mod(a, 10), mod(b, 10))

  Belt.Result.cmp(Ok(39), Ok(57), mod10cmp) == 1

  Belt.Result.cmp(Ok(57), Ok(39), mod10cmp) == (-1)

  Belt.Result.cmp(Ok(39), Error("y"), mod10cmp) == 1

  Belt.Result.cmp(Error("x"), Ok(57), mod10cmp) == (-1)

  Belt.Result.cmp(Error("x"), Error("y"), mod10cmp) == 0
  ```
*)
