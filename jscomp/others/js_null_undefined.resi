/*** Contains functionality for dealing with values that can be both `null` and `undefined` */

/** Local alias for `Js.null_undefined('a)`. */
type t<+'a> = Js.null_undefined<'a>

/** Constructs a value of `Js.null_undefined('a)` containing a value of `'a`. */
external return: 'a => t<'a> = "%identity"

/** Returns `true` if the given value is null or undefined, `false` otherwise. */
external isNullable: t<'a> => bool = "#is_nullable"

/** The null value of type `Js.null_undefined('a)`. */
external null: t<'a> = "#null"

/** The undefined value of type `Js.null_undefined('a)`. */
external undefined: t<'a> = "#undefined"

/**
Maps the contained value using the given function.

If `Js.null_undefined('a)` contains a value, that value is unwrapped, mapped to
a `'b` using the given function `a' => 'b`, then wrapped back up and returned
as `Js.null_undefined('b)`.

```res example
let maybeGreetWorld = (maybeGreeting: Js.null_undefined<string>) =>
  Js.Null_undefined.bind(maybeGreeting, (. greeting) => greeting ++ \" world!\")
```
*/
let bind: (t<'a>, (. 'a) => 'b) => t<'b>

/**
Iterates over the contained value with the given function.
If `Js.null_undefined('a)` contains a value, that value is unwrapped and applied to the given function.

```res example
let maybeSay = (maybeMessage: Js.null_undefined<string>) =>
  Js.Null_undefined.iter(maybeMessage, (. message) => Js.log(message))
```
*/
let iter: (t<'a>, (. 'a) => unit) => unit

/**
Maps `option('a)` to `Js.null_undefined('a)`.
`Some(a)` => `a`
`None` => `undefined`
*/
let fromOption: option<'a> => t<'a>

@deprecated("Use fromOption instead") let from_opt: option<'a> => t<'a>

/**
Maps `Js.null_undefined('a)` to `option('a)`.
`a` => `Some(a)`
`undefined` => `None`
`null` => `None`
*/
external toOption: t<'a> => option<'a> = "#nullable_to_opt"

@deprecated("Use toOption instead") external to_opt: t<'a> => option<'a> = "#nullable_to_opt"
