(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

(** Contains functionality for dealing with values that can be both `null` and `undefined` *)

type +'a t = 'a Js.null_undefined
(** Local alias for `Js.null_undefined('a)`. *)

external return : 'a -> 'a t = "%identity"
(** Constructs a value of `Js.null_undefined('a)` containing a value of `'a`. *)

external isNullable : 'a t -> bool = "#is_nullable"
(** Returns `true` if the given value is null or undefined, `false` otherwise. *)

external null : 'a t = "#null"
(** The null value of type `Js.null_undefined('a)`. *)

external undefined : 'a t = "#undefined"
(** The undefined value of type `Js.null_undefined('a)`. *)

val bind : 'a t -> (('a -> 'b)[@bs]) -> 'b t
(**
Maps the contained value using the given function.

If `Js.null_undefined('a)` contains a value, that value is unwrapped, mapped to
a `'b` using the given function `a' => 'b`, then wrapped back up and returned
as `Js.null_undefined('b)`.

```res example
let maybeGreetWorld = (maybeGreeting: Js.null_undefined<string>) =>
  Js.Null_undefined.bind(maybeGreeting, (. greeting) => greeting ++ " world!")
```
*)

val iter : 'a t -> (('a -> unit)[@bs]) -> unit
(**
Iterates over the contained value with the given function.
If `Js.null_undefined('a)` contains a value, that value is unwrapped and applied to the given function.

```res example
let maybeSay = (maybeMessage: Js.null_undefined<string>) =>
  Js.Null_undefined.iter(maybeMessage, (. message) => Js.log(message))
```
*)

val fromOption : 'a option -> 'a t
(**
Maps `option('a)` to `Js.null_undefined('a)`.
`Some(a)` => `a`
`None` => `undefined`
*)

val from_opt : 'a option -> 'a t [@@deprecated "Use fromOption instead"]

external toOption : 'a t -> 'a option = "#nullable_to_opt"
(**
Maps `Js.null_undefined('a)` to `option('a)`.
`a` => `Some(a)`
`undefined` => `None`
`null` => `None`
*)

external to_opt : 'a t -> 'a option = "#nullable_to_opt"
  [@@deprecated "Use toOption instead"]
