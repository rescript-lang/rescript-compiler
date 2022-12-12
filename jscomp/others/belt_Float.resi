/*** This module includes convenience methods for handling `float` types. */

/**
Converts a given `float` to an `int`.

```res example
Js.log(Belt.Float.toInt(1.0) === 1) /* true */
```
*/
external toInt: float => int = "%intoffloat"

/** 
  Converts a given `int` to a `float`.

  ```res example
  Js.log(Belt.Float.fromInt(1) === 1.0) /* true */
  ```
*/
external fromInt: int => float = "%identity"

/** 
  Converts a given `string` to a `float`. Returns `Some(float)` when the input is a number, `None` otherwise.

  ```res example
  Js.log(Belt.Float.fromString(\"1.0\") === Some(1.0)) /* true */
  ```
*/
let fromString: string => option<float>

@val
/**
  Converts a given `float` to a `string`. Uses the JavaScript `String` constructor under the hood.

  ```res example
  Js.log(Belt.Float.toString(1.0) === \"1.0\") /* true */
  ```
*/
external toString: float => string = "String"

/**
  Addition of two `float` values.
  Can be opened in a module to avoid dot-notation (`+.`), however this yields a shadow warning (Warning number 44) in the default configuration.

  ```res example
  open Belt.Float
  Js.log(2.0 + 2.0 === 4.0) /* true */
  ```
*/
external \"+": (float, float) => float = "%addfloat"

/**
  Subtraction of two `float` values.
  Can be opened in a module to avoid dot-notation (`-.`), however this yields a shadow warning (Warning number 44) in the default configuration.

  ```res example
  open Belt.Float
  Js.log(2.0 - 1.0 === 1.0) /* true */
  ```
*/
external \"-": (float, float) => float = "%subfloat"

/**
  Multiplication of two `float` values.
  Can be opened in a module to avoid dot-notation (`*.`), however this yields a shadow warning (Warning number 44) in the default configuration.

  ```res example
  open Belt.Float
  Js.log(2.0 * 2.0 === 4.0) /* true */
  ```
*/
external \"*": (float, float) => float = "%mulfloat"

/** 
  Division of two `float` values.
  Can be opened in a module to avoid dot-notation (`/.`), however this yields a shadow warning (Warning number 44) in the default configuration.

  ```res example
  open Belt.Float
  Js.log(4.0 / 2.0 === 2.0) /* true */
  ```
*/
external \"/": (float, float) => float = "%divfloat"
