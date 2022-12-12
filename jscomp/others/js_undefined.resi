/*** Provides functionality for dealing with the `Js.undefined('a)` type */

/** Local alias for `Js.undefined('a)` */
type t<+'a> = Js.undefined<'a>

/** Constructs a value of `Js.undefined('a)` containing a value of `'a`. */
external return: 'a => t<'a> = "%identity"

@deprecated("Use = Js.undefined directly")
/** Returns `true` if the given value is empty (undefined), `false` otherwise. */
let test: t<'a> => bool

/**
Returns `true` if the given value is empty (undefined).

**since 1.6.1**
*/
let testAny: 'a => bool

/** The empty value, `undefined` */
external empty: t<'a> = "#undefined"

external getUnsafe: t<'a> => 'a = "%identity"
let getExn: t<'a> => 'a

/**
Maps the contained value using the given function.
If `Js.undefined('a)` contains a value, that value is unwrapped, mapped to a
`'b` using the given function `a' => 'b`, then wrapped back up and returned as
`Js.undefined('b)`.

```res example
let maybeGreetWorld = (maybeGreeting: Js.undefined<string>) =>
  Js.Undefined.bind(maybeGreeting, (. greeting) => greeting ++ \" world!\")
```
*/
let bind: (t<'a>, (. 'a) => 'b) => t<'b>

/**
Iterates over the contained value with the given function. If
`Js.undefined('a)` contains a value, that value is unwrapped and applied to the
given function.

```res example
let maybeSay = (maybeMessage: Js.undefined<string>) =>
  Js.Undefined.iter(maybeMessage, (. message) => Js.log(message))
```
*/
let iter: (t<'a>, (. 'a) => unit) => unit

/**
Maps `option('a)` to `Js.undefined('a)`.
`Some(a)` => `a`
`None` => `empty`
*/
let fromOption: option<'a> => t<'a>

@deprecated("Use fromOption instead") let from_opt: option<'a> => t<'a>

/**
Maps `Js.undefined('a)` to `option('a)`
`a` => `Some(a)`
`empty` => `None`
*/
external toOption: t<'a> => option<'a> = "#undefined_to_opt"

@deprecated("use toOption instead") external to_opt: t<'a> => option<'a> = "#undefined_to_opt"
