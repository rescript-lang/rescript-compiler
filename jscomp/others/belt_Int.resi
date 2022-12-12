/***
  This module includes convenience methods for handling `int` types.
*/

/**
  Converts a given `int` to a `float`.

  ```res example
  Js.log(Belt.Int.toFloat(1) === 1.0) /* true */
  ```
*/
external toFloat: int => float = "%identity"

/**
  Converts a given `float` to an `int`.

  ```res example
  Js.log(Belt.Int.fromFloat(1.0) === 1) /* true */
  ```
*/
external fromFloat: float => int = "%intoffloat"

/**
  Converts a given `string` to an `int`. Returns `Some(int)` when the input is a number, `None` otherwise.

  ```res example
  Js.log(Belt.Int.fromString(\"1\") === Some(1)) /* true */
  ```
*/
let fromString: string => option<int>

/**
  Converts a given `int` to a `string`. Uses the JavaScript `String` constructor under the hood.

  ```res example
  Js.log(Belt.Int.toString(1) === \"1\") /* true */
  ```
*/
@val
external toString: int => string = "String"

/**
  Addition of two `int` values. Same as the addition from `Pervasives`.

  ```res example
  open Belt.Int
  Js.log(2 + 2 === 4) /* true */
  ```
*/
external \"+": (int, int) => int = "%addint"

/**
  Subtraction of two `int` values. Same as the subtraction from `Pervasives`.

  ```res example
  open Belt.Int
  Js.log(2 - 1 === 1) /* true */
  ```
*/
external \"-": (int, int) => int = "%subint"

/**
  Multiplication of two `int` values. Same as the multiplication from `Pervasives`.

  ```res example
  open Belt.Int
  Js.log(2 * 2 === 4) /* true */
  ```
*/
external \"*": (int, int) => int = "%mulint"

/**
  Division of two `int` values. Same as the division from `Pervasives`.

  ```res example
  open Belt.Int
  Js.log(4 / 2 === 2); /* true */
  ```
*/
external \"/": (int, int) => int = "%divint"
