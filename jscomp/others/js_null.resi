/*** Provides functionality for dealing with the `'a Js.null` type */

/*** Provides functionality for dealing with the `Js.null('a)` type */

/** Local alias for `Js.null('a)` */
type t<+'a> = Js.null<'a>

/** Constructs a value of `Js.null('a)` containing a value of `'a`. */
external return: 'a => t<'a> = "%identity"

@deprecated("Use = Js.null directly ")
/** Returns `true` if the given value is empty (`null`), `false` otherwise. */
let test: t<'a> => bool

/** The empty value, `null` */
external empty: t<'a> = "#null"

external getUnsafe: t<'a> => 'a = "%identity"
let getExn: t<'a> => 'a

/**
Maps the contained value using the given function.

If `Js.null('a)` contains a value, that value is unwrapped, mapped to a `'b`
using the given function `'a => 'b`, then wrapped back up and returned as
`Js.null('b)`.

```res example
let maybeGreetWorld = (maybeGreeting: Js.null<string>) =>
  Js.Null.bind(maybeGreeting, (. greeting) => greeting ++ \" world!\")
```
*/
let bind: (t<'a>, (. 'a) => 'b) => t<'b>

/**
Iterates over the contained value with the given function.
If `Js.null('a)` contains a value, that value is unwrapped and applied to the given function.

```res example
let maybeSay = (maybeMessage: Js.null<string>) =>
  Js.Null.iter(maybeMessage, (. message) => Js.log(message))
```
*/
let iter: (t<'a>, (. 'a) => unit) => unit

/**
Maps `option('a)` to `Js.null('a)`.
`Some(a)` => `a`
`None` => `empty`
*/
let fromOption: option<'a> => t<'a>

@deprecated("Use fromOption instead") let from_opt: option<'a> => t<'a>

/**
Maps `Js.null('a)` to `option('a)`.
`a` => `Some(a)`
`empty` => `None`
*/
external toOption: t<'a> => option<'a> = "#null_to_opt"

@deprecated("Use toOption instead") external to_opt: t<'a> => option<'a> = "#null_to_opt"
