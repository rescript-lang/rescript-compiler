/*** JavaScript BigInt API */

@val
/**
The special value "Not a Number". See [`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN) on MDN.
*/
external _NaN: bigint = "NaN"

@val
/**
Parses the given `string` into a `bigint` using JavaScript semantics. Return the
number as a `bigint` if successfully parsed, `null`, `undefined`, `_NaN` otherwise.

## Examples

```rescript
/* returns 123n */
Js.Bigint.fromString("123")

/* returns 0n */
Js.Bigint.fromString("")

/* returns 17n */
Js.Bigint.fromString("0x11")

/* returns 3n */
Js.Bigint.fromString("0b11")

/* returns 9n */
Js.Bigint.fromString("0o11")
```
*/
external fromStringExn: string => bigint = "BigInt"

// Operations

external \"~-": bigint => bigint = "%negbigint"
external \"~+": bigint => bigint = "%identity"
external \"+": (bigint, bigint) => bigint = "%addbigint"
external \"-": (bigint, bigint) => bigint = "%subbigint"
external \"*": (bigint, bigint) => bigint = "%mulbigint"
external \"/": (bigint, bigint) => bigint = "%divbigint"
external mod: (bigint, bigint) => bigint = "%modbigint"
external \"**": (bigint, bigint) => bigint = "%powbigint"

@send
/**
Formats a `bigint` as a string. Return a `string` representing the given value.
See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
/* prints "123" */
Js.Bigint.toString(123n)->Js.log
```
*/
external toString: bigint => string = "toString"

@send
/**
Returns a string with a language-sensitive representation of this Bigint value.

## Examples

```rescript
/* prints "123" */
Js.Bigint.toString(123n)->Js.log
```
*/
external toLocaleString: bigint => string = "toLocaleString"
