/*** JavaScript BigInt API */

@val
/**
The special value "Not a Number". See [`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN) on MDN.
*/
external _NaN: bigint = "NaN"

@val
@scope("Number")
/**
Tests if the given value is `_NaN`

Note that both `_NaN = _NaN` and `_NaN == _NaN` will return `false`. `isNaN` is
therefore necessary to test for `_NaN`. Return `true` if the given value is
`_NaN`, `false` otherwise. See [`isNaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isNaN) on MDN.
*/
external isNaN: bigint => bool = "isNaN"

/**
Tests if the given value is finite. Return `true` if the given value is a finite
number, `false` otherwise. See [`isFinite`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isFinite) on MDN.

## Examples

```rescript
/* returns [false] */
Js.Bigint.isFinite(infinity)

/* returns [false] */
Js.Bigint.isFinite(neg_infinity)

/* returns [false] */
Js.Bigint.isFinite(Js.Bigint._NaN)

/* returns [true] */
Js.Bigint.isFinite(1234.)
```
*/
external isFinite: bigint => bool = "isFinite"

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
