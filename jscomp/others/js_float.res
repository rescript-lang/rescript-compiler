/*** Provide utilities for JS float. */

@val
/**
  The special value \"Not a Number\"

  **See:** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN)
*/
external _NaN: float = "NaN"

@val
@scope("Number")
/**
  Tests if the given value is `_NaN`

  Note that both `_NaN = _NaN` and `_NaN == _NaN` will return `false`. `isNaN` is
  therefore necessary to test for `_NaN`.

  **return** `true` if the given value is `_NaN`, `false` otherwise

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isNaN)
*/
external isNaN: float => bool = "isNaN"

@val
@scope("Number")
/**
  Tests if the given value is finite

  **return** `true` if the given value is a finite number, `false` otherwise

  ```res example
  /* returns [false] */
  Js.Float.isFinite(infinity)

  /* returns [false] */
  Js.Float.isFinite(neg_infinity)

  /* returns [false] */
  Js.Float.isFinite(Js.Float._NaN)

  /* returns [true] */
  Js.Float.isFinite(1234.)
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isFinite)
*/
external isFinite: float => bool = "isFinite"

@send
/**
  Formats a `float` using exponential (scientific) notation

  **return** a `string` representing the given value in exponential notation

  **raise** RangeError if digits is not in the range [0, 20] (inclusive)

  ```res example
  /* prints \"7.71234e+1\" */
  Js.Float.toExponential(77.1234)->Js.log

  /* prints \"7.7e+1\" */
  Js.Float.toExponential(77.)->Js.log
  ```

  **See** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential)
*/
external toExponential: float => string = "toExponential"

@send
/**
  Formats a `float` using exponential (scientific) notation

  **digits** specifies how many digits should appear after the decimal point. The
  value must be in the range [0, 20] (inclusive).

  **return** a `string` representing the given value in exponential notation

  The output will be rounded or padded with zeroes if necessary.

  **raise** RangeError if digits is not in the range [0, 20] (inclusive)

  ```res example
  /* prints \"7.71e+1\" */
  Js.Float.toExponentialWithPrecision(77.1234, ~digits=2)->Js.log
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential)
*/
external toExponentialWithPrecision: (float, ~digits: int) => string = "toExponential"

@send
/**
  Formats a `float` using fixed point notation

  **return** a `string` representing the given value in fixed-point notation (usually)

  **raise** RangeError if digits is not in the range [0, 20] (inclusive)

  ```res example
  /* prints \"12346\" (note the rounding) */
  Js.Float.toFixed(12345.6789)->Js.log

  /* print \"1.2e+21\" */
  Js.Float.toFixed(1.2e21)->Js.log
  ```

  **See** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
*/
external toFixed: float => string = "toFixed"

@send
/**
  Formats a `float` using fixed point notation

  **digits** specifies how many digits should appear after the decimal point. The
  value must be in the range [0, 20] (inclusive). Defaults to `0`.

  **return** a `string` representing the given value in fixed-point notation (usually)

  The output will be rounded or padded with zeroes if necessary.

  **raise** RangeError if digits is not in the range [0, 20] (inclusive)

  ```res example
  /* prints \"12345.7\" (note the rounding) */
  Js.Float.toFixedWithPrecision(12345.6789, ~digits=1)->Js.log

  /* prints \"0.00\" (note the added zeroes) */
  Js.Float.toFixedWithPrecision(0., ~digits=2)->Js.log
  ```

  **See** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
*/
external toFixedWithPrecision: (float, ~digits: int) => string = "toFixed"

@send
/**
  Formats a `float` using some fairly arbitrary rules

  **return** a `string` representing the given value in fixed-point (usually)

  `toPrecision` differs from `toFixed` in that the former will format the number
  with full precision, while the latter will not output any digits after the
  decimal point.

  **raise** RangeError if digits is not in the range accepted by this function (what do you mean \"vague\"?)

  ```res example
  /* prints \"12345.6789\" */
  Js.Float.toPrecision(12345.6789)->Js.log

  /* print \"1.2e+21\" */
  Js.Float.toPrecision(1.2e21)->Js.log
  ```

  **See** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision)
*/
external toPrecision: float => string = "toPrecision"

/* equivalent to `toString` I think */

@send
/**
  Formats a `float` using some fairly arbitrary rules

  **digits** specifies how many digits should appear in total. The
  value must between 0 and some arbitrary number that's hopefully at least larger
  than 20 (for Node it's 21. Why? Who knows).

  **return** a `string` representing the given value in fixed-point or scientific notation

  The output will be rounded or padded with zeroes if necessary.

  `toPrecisionWithPrecision` differs from `toFixedWithPrecision` in that the former
  will count all digits against the precision, while the latter will count only
  the digits after the decimal point. `toPrecisionWithPrecision` will also use
  scientific notation if the specified precision is less than the number for digits
  before the decimal point.

  **raise** RangeError if digits is not in the range accepted by this function (what do you mean \"vague\"?)

  ```res example
  /* prints \"1e+4\" */
  Js.Float.toPrecisionWithPrecision(12345.6789, ~digits=1)->Js.log

  /* prints \"0.0\" */
  Js.Float.toPrecisionWithPrecision(0., ~digits=2)->Js.log
  ```

  **See** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision)
*/
external toPrecisionWithPrecision: (float, ~digits: int) => string = "toPrecision"

@send
/**
  Formats a `float` as a string

  **return** a `string` representing the given value in fixed-point (usually)

  ```res example
  /* prints \"12345.6789\" */
  Js.Float.toString(12345.6789)->Js.log
  ```

  **See** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString)
*/
external toString: float => string = "toString"

@send
/**
  Formats a `float` as a string

  **radix** specifies the radix base to use for the formatted number. The
  value must be in the range [2, 36] (inclusive).

  **return** a `string` representing the given value in fixed-point (usually)

  **raise** RangeError if radix is not in the range [2, 36] (inclusive)

  ```res example
  /* prints \"110\" */
  Js.Float.toStringWithRadix(6., ~radix=2)->Js.log

  /* prints \"11.001000111101011100001010001111010111000010100011111\" */
  Js.Float.toStringWithRadix(3.14, ~radix=2)->Js.log

  /* prints \"deadbeef\" */
  Js.Float.toStringWithRadix(3735928559., ~radix=16)->Js.log

  /* prints \"3f.gez4w97ry0a18ymf6qadcxr\" */
  Js.Float.toStringWithRadix(123.456, ~radix=36)->Js.log
  ```

  **See** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString)
*/
external toStringWithRadix: (float, ~radix: int) => string = "toString"

@val
/**
  Parses the given `string` into a `float` using JavaScript semantics

  **return** the number as a `float` if successfully parsed, `_NaN` otherwise.

  ```res example
  /* returns 123 */
  Js.Float.fromString(\"123\")

  /* returns 12.3 */
  Js.Float.fromString(\"12.3\")

  /* returns 0 */
  Js.Float.fromString(\"\")

  /* returns 17 */
  Js.Float.fromString(\"0x11\")

  /* returns 3 */
  Js.Float.fromString(\"0b11\")

  /* returns 9 */
  Js.Float.fromString(\"0o11\")

  /* returns [_NaN] */
  Js.Float.fromString(\"hello\")

  /* returns [_NaN] */
  Js.Float.fromString(\"100a\")
  ```
*/
external fromString: string => float = "Number"
