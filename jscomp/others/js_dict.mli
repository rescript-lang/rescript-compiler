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

(**
Provide utilities for JS dictionary object.

**Note:** This module's examples will assume this predeclared dictionary:

```res prelude
let ages = Js.Dict.fromList(list{("Maria", 30), ("Vinh", 22), ("Fred", 49)})
```
*)

type 'a t
(**
Dictionary type (ie an '{ }' JS object). However it is restricted to hold a
single type; therefore values must have the same type. This Dictionary type is
mostly used with the Js_json.t type.
*)

type key = string
(** The type for dictionary keys. This means that dictionaries *must* use `string`s as their keys. *)

val get : 'a t -> key -> 'a option
(**
`Js.Dict.get(key)` returns `None` if the key is not found in the dictionary,
`Some(value)` otherwise.

```res example
Js.Dict.get(ages, "Vinh") == Some(22)
Js.Dict.get(ages, "Paul") == None
```
*)

external unsafeGet : 'a t -> key -> 'a = ""
  [@@bs.get_index]
(**
`Js.Dict.unsafeGet(key)` returns the value if the key exists, otherwise an `undefined` value is returned. Use this only when you are sure the key exists (i.e. when having used the `keys()` function to check that the key is valid).

```res example
Js.Dict.unsafeGet(ages, "Fred") == 49
Js.Dict.unsafeGet(ages, "Paul") // returns undefined
```
*)

external set : 'a t -> key -> 'a -> unit = ""
  [@@bs.set_index]
(**
`Js.Dict.set(dict, key, value)` sets the key/value in the dictionary `dict`. If
the key does not exist, and entry will be created for it.

*This function modifies the original dictionary.*

```res example
Js.Dict.set(ages, "Maria", 31)
Js.log(ages == Js.Dict.fromList(list{("Maria", 31), ("Vinh", 22), ("Fred", 49)}))

Js.Dict.set(ages, "David", 66)
Js.log(ages == Js.Dict.fromList(list{("Maria", 31), ("Vinh", 22), ("Fred", 49), ("David", 66)}))
```
*)

external keys : 'a t -> string array = "Object.keys"
  [@@bs.val]
(**
Returns all the keys in the dictionary `dict`.

```res example
Js.Dict.keys(ages) == ["Maria", "Vinh", "Fred"]
```
*)

external empty : unit -> 'a t = ""
  [@@bs.obj]
(** Returns an empty dictionary. *)

val unsafeDeleteKey : (string t -> string -> unit[@bs])
(** Experimental internal function *)

val entries : 'a t -> (key * 'a) array
(**
Returns an array of key/value pairs in the given dictionary (ES2017).

```res example
Js.Dict.entries(ages) == [("Maria", 30), ("Vinh", 22), ("Fred", 49)]
```
*)

val values : 'a t -> 'a array
(**
Returns the values in the given dictionary (ES2017).

```res example
Js.Dict.values(ages) == [30, 22, 49]
```
*)

val fromList : (key * 'a) list -> 'a t
(**
Creates a new dictionary containing each (key, value) pair in its list
argument.

```res example
let capitals = Js.Dict.fromList(list{("Japan", "Tokyo"), ("France", "Paris"), ("Egypt", "Cairo")})
```
*)

val fromArray : (key * 'a) array -> 'a t
(**
Creates a new dictionary containing each (key, value) pair in its array
argument.

```res example
let capitals2 = Js.Dict.fromArray([("Germany", "Berlin"), ("Burkina Faso", "Ouagadougou")])
```
*)

val map : (('a -> 'b)[@bs]) -> 'a t -> 'b t
(**
`map(f, dict)` maps `dict` to a new dictionary with the same keys, using the
function `f` to map each value.

```res example
let prices = Js.Dict.fromList(list{("pen", 1.00), ("book", 5.00), ("stapler", 7.00)})

let discount = (. price) => price *. 0.90
let salePrices = Js.Dict.map(discount, prices)

salePrices == Js.Dict.fromList(list{("pen", 0.90), ("book", 4.50), ("stapler", 6.30)})
```
*)
