/***
Provide utilities for JS dictionary object.

**Note:** This module's examples will assume this predeclared dictionary:

```res prelude
let ages = Js.Dict.fromList(list{(\"Maria\", 30), (\"Vinh\", 22), (\"Fred\", 49)})
```
*/

/**
Dictionary type (ie an '{ }' JS object). However it is restricted to hold a
single type; therefore values must have the same type. This Dictionary type is
mostly used with the Js_json.t type.
*/
type t<'a>

/** The type for dictionary keys. This means that dictionaries *must* use `string`s as their keys. */
type key = string

/**
`Js.Dict.get(key)` returns `None` if the key is not found in the dictionary,
`Some(value)` otherwise.

```res example
Js.Dict.get(ages, \"Vinh\") == Some(22)
Js.Dict.get(ages, \"Paul\") == None
```
*/
let get: (t<'a>, key) => option<'a>

@get_index
/**
`Js.Dict.unsafeGet(key)` returns the value if the key exists, otherwise an `undefined` value is returned. Use this only when you are sure the key exists (i.e. when having used the `keys()` function to check that the key is valid).

```res example
Js.Dict.unsafeGet(ages, \"Fred\") == 49
Js.Dict.unsafeGet(ages, \"Paul\") // returns undefined
```
*/
external unsafeGet: (t<'a>, key) => 'a = ""

@set_index
/**
`Js.Dict.set(dict, key, value)` sets the key/value in the dictionary `dict`. If
the key does not exist, and entry will be created for it.

*This function modifies the original dictionary.*

```res example
Js.Dict.set(ages, \"Maria\", 31)
Js.log(ages == Js.Dict.fromList(list{(\"Maria\", 31), (\"Vinh\", 22), (\"Fred\", 49)}))

Js.Dict.set(ages, \"David\", 66)
Js.log(ages == Js.Dict.fromList(list{(\"Maria\", 31), (\"Vinh\", 22), (\"Fred\", 49), (\"David\", 66)}))
```
*/
external set: (t<'a>, key, 'a) => unit = ""

@val
/**
Returns all the keys in the dictionary `dict`.

```res example
Js.Dict.keys(ages) == [\"Maria\", \"Vinh\", \"Fred\"]
```
*/
external keys: t<'a> => array<string> = "Object.keys"

@obj /** Returns an empty dictionary. */
external empty: unit => t<'a> = ""

/** Experimental internal function */
let unsafeDeleteKey: (. t<string>, string) => unit

/**
Returns an array of key/value pairs in the given dictionary (ES2017).

```res example
Js.Dict.entries(ages) == [(\"Maria\", 30), (\"Vinh\", 22), (\"Fred\", 49)]
```
*/
let entries: t<'a> => array<(key, 'a)>

/**
Returns the values in the given dictionary (ES2017).

```res example
Js.Dict.values(ages) == [30, 22, 49]
```
*/
let values: t<'a> => array<'a>

/**
Creates a new dictionary containing each (key, value) pair in its list
argument.

```res example
let capitals = Js.Dict.fromList(list{(\"Japan\", \"Tokyo\"), (\"France\", \"Paris\"), (\"Egypt\", \"Cairo\")})
```
*/
let fromList: list<(key, 'a)> => t<'a>

/**
Creates a new dictionary containing each (key, value) pair in its array
argument.

```res example
let capitals2 = Js.Dict.fromArray([(\"Germany\", \"Berlin\"), (\"Burkina Faso\", \"Ouagadougou\")])
```
*/
let fromArray: array<(key, 'a)> => t<'a>

/**
`map(f, dict)` maps `dict` to a new dictionary with the same keys, using the
function `f` to map each value.

```res example
let prices = Js.Dict.fromList(list{(\"pen\", 1.00), (\"book\", 5.00), (\"stapler\", 7.00)})

let discount = (. price) => price *. 0.90
let salePrices = Js.Dict.map(discount, prices)

salePrices == Js.Dict.fromList(list{(\"pen\", 0.90), (\"book\", 4.50), (\"stapler\", 6.30)})
```
*/
let map: ((. 'a) => 'b, t<'a>) => t<'b>
