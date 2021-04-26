(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** Provides functions for inspecting and manipulating `float`s *)

(**
  The special value "Not a Number"

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN)
*)
external _NaN : float = "NaN" [@@bs.val]

(**
  Tests if the given value is `_NaN`

  Note that both `_NaN = _NaN` and `_NaN == _NaN` will return `false`. `isNaN` is
  therefore necessary to test for `_NaN`.

  **return** `true` if the given value is `_NaN`, `false` otherwise

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isNaN)
*)
external isNaN : float -> bool = "isNaN" [@@bs.val] [@@bs.scope "Number"]

(**
  Tests if the given value is finite

  **return** `true` if the given value is a finite number, `false` otherwise

  ```
  (* returns `false` *)
  let _ = Js.Float.isFinite infinity

  (* returns `false` *)
  let _ = Js.Float.isFinite neg_infinity

  (* returns `false` *)
  let _ = Js.Float.isFinite _NaN

  (* returns `true` *)
  let _ = Js.Float.isFinite 1234
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isFinite)
*)
external isFinite : float -> bool = "isFinite" [@@bs.val] [@@bs.scope "Number"]

(**
  Formats a `float` using exponential (scientific) notation

  **return** a `string` representing the given value in exponential notation

  **raise** RangeError if digits is not in the range [0, 20] (inclusive)

  ```
  (* prints "7.71234e+1" *)
  let _ = Js.log @@ Js.Float.toExponential 77.1234

  (* prints "7.7e+1" *)
  let _ = Js.log @@ Js.Float.toExponential 77.
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential)
*)
external toExponential : float -> string = "toExponential" [@@bs.send]

(**
  Formats a `float` using exponential (scientific) notation

  **digits** specifies how many digits should appear after the decimal point. The
  value must be in the range [0, 20] (inclusive).

  **return** a `string` representing the given value in exponential notation

  The output will be rounded or padded with zeroes if necessary.

  **raise** RangeError if digits is not in the range [0, 20] (inclusive)

  ```
  (* prints "7.71e+1" *)
  let _ = Js.log @@ Js.Float.toExponentialWithPrecision 77.1234 ~digits:2
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential)
*)
external toExponentialWithPrecision : float -> digits:int -> string = "toExponential" [@@bs.send]

(**
  Formats a `float` using fixed point notation

  **return** a `string` representing the given value in fixed-point notation (usually)

  **raise** RangeError if digits is not in the range [0, 20] (inclusive)

  ```
  (* prints "12346" (note the rounding) *)
  let _ = Js.log @@ Js.Float.toFixed 12345.6789

  (* print "1.2e+21" *)
  let _ = Js.log @@ Js.Float.toFixed 1.2e21
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
*)
external toFixed : float -> string = "toFixed" [@@bs.send]

(**
  Formats a `float` using fixed point notation

  **digits** specifies how many digits should appear after the decimal point. The
  value must be in the range [0, 20] (inclusive). Defaults to `0`.

  **return** a `string` representing the given value in fixed-point notation (usually)

  The output will be rounded or padded with zeroes if necessary.

  **raise** RangeError if digits is not in the range [0, 20] (inclusive)

  ```
  (* prints "12345.7" (note the rounding) *)
  let _ = Js.log @@ Js.Float.toFixedWithPrecision 12345.6789 ~digits:1

  (* prints "0.00" (note the added zeroes) *)
  let _ = Js.log @@ Js.Float.toFixedWithPrecision 0. ~digits:2
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
*)
external toFixedWithPrecision : float -> digits:int -> string = "toFixed" [@@bs.send]

(**
  Formats a `float` using some fairly arbitrary rules

  **return** a `string` representing the given value in fixed-point (usually)

  `toPrecision` differs from `toFixed` in that the former will format the number
  with full precision, while the latter will not output any digits after the
  decimal point.

  **raise** RangeError if digits is not in the range accepted by this function (what do you mean "vague"?)

  ```
  (* prints "12345.6789" *)
  let _ = Js.log @@ Js.Float.toPrecision 12345.6789

  (* print "1.2e+21" *)
  let _ = Js.log @@ Js.Float.toPrecision 1.2e21
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision)
*)
external toPrecision : float -> string = "toPrecision" [@@bs.send] (* equivalent to `toString` I think *)

(**
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

  **raise** RangeError if digits is not in the range accepted by this function (what do you mean "vague"?)

  ```
  (* prints "1e+4" *)
  let _ = Js.log @@ Js.Float.toPrecisionWithPrecision 12345.6789 ~digits:1

  (* prints "0.0" *)
  let _ = Js.log @@ Js.Float.toPrecisionWithPrecision 0. ~digits:2
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision)
*)
external toPrecisionWithPrecision : float -> digits:int -> string = "toPrecision" [@@bs.send]


(**
  Formats a `float` as a string

  **return** a `string` representing the given value in fixed-point (usually)

  ```
  (* prints "12345.6789" *)
  let _ = Js.log @@ Js.Float.toString 12345.6789
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString)
*)
external toString : float -> string = "toString" [@@bs.send]

(**
  Formats a `float` as a string

  **radix** specifies the radix base to use for the formatted number. The
  value must be in the range [2, 36] (inclusive).

  **return** a `string` representing the given value in fixed-point (usually)

  **raise** RangeError if radix is not in the range [2, 36] (inclusive)

  ```
  (* prints "110" *)
  let _ = Js.log @@ Js.Float.toStringWithRadix 6. ~radix:2

  (* prints "11.001000111101011100001010001111010111000010100011111" *)
  let _ = Js.log @@ Js.Float.toStringWithRadix 3.14 ~radix:2

  (* prints "deadbeef" *)
  let _ = Js.log @@ Js.Float.toStringWithRadix 3735928559. ~radix:16

  (* prints "3f.gez4w97ry0a18ymf6qadcxr" *)
  let _ = Js.log @@ Js.Float.toStringWithRadix 123.456 ~radix:36
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString)
*)
external toStringWithRadix : float -> radix:int -> string = "toString" [@@bs.send]

(**
  Parses the given `string` into a `float` using JavaScript semantics

  **return** the number as a `float` if successfully parsed, `_NaN` otherwise.

  ```
  (* returns 123 *)
  let _ = Js.Float.fromString "123"

  (* returns 12.3 *)
  let _ = Js.Float.fromString "12.3"

  (* returns 0 *)
  let _ = Js.Float.fromString ""

  (* returns 17 *)
  let _ = Js.Float.fromString "0x11"

  (* returns 3 *)
  let _ = Js.Float.fromString "0b11"

  (* returns 9 *)
  let _ = Js.Float.fromString "0o11"

  (* returns `_NaN` *)
  let _ = Js.Float.fromString "foo"

  (* returns `_NaN` *)
  let _ = Js.Float.fromString "100a"
  ```
*)
external fromString : string -> float = "Number" [@@bs.val]


