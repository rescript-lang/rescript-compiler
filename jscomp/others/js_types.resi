/*** Provide utilities for manipulating JS types. */

/** Js symbol type (only available in ES6) */
type symbol

/** Js bigint type only available in ES2020 */
type bigint_val

type obj_val

/** This type has only one value `undefined` */
type undefined_val

/** This type has only one value `null` */
type null_val

type function_val

type rec t<_> =
  | Undefined: t<undefined_val>
  | Null: t<null_val>
  | Boolean: t<bool>
  | Number: t<float>
  | String: t<string>
  | Function: t<function_val>
  | Object: t<obj_val>
  | Symbol: t<symbol>
  | BigInt: t<bigint_val>

/**
   `test(value, t)` returns `true` if `value` is `typeof t`, otherwise `false`.
   This is useful for doing runtime reflection on any given value.

   ```res example
   test(\"test\", String) == true
   test(() => true, Function) == true
   test(\"test\", Boolean) == false
   ```
*/
let test: ('a, t<'b>) => bool

type tagged_t =
  | JSFalse
  | JSTrue
  | JSNull
  | JSUndefined
  | JSNumber(float)
  | JSString(string)
  | JSFunction(function_val)
  | JSObject(obj_val)
  | JSSymbol(symbol)
  | JSBigInt(bigint_val)

let classify: 'a => tagged_t
