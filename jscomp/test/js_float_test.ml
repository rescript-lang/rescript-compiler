open Js_float

let suites = Mt.[
  ("_NaN <> _NaN", (fun _ ->
    Eq(false, _NaN = _NaN)));

  ("isNaN - _NaN", (fun _ ->
    Eq(true, isNaN _NaN)));
  ("isNaN - 0.", (fun _ ->
    Eq(false, isNaN 0.)));

  ("isFinite - infinity", (fun _ ->
    Eq(false, isFinite infinity)));
  ("isFinite - neg_infinity", (fun _ ->
    Eq(false, isFinite neg_infinity)));
  ("isFinite - _NaN", (fun _ ->
    Eq(false, isFinite _NaN)));
  ("isFinite - 0.", (fun _ ->
    Eq(true, isFinite 0.)));

  ("toExponential", (fun _ ->
    Eq("1.23456e+2", toExponential 123.456)));
  ("toExponential - large number", (fun _ ->
    Eq("1.2e+21", toExponential 1.2e21)));
  ("toExponentialWithPrecision - digits:2", (fun _ ->
    Eq("1.23e+2", toExponentialWithPrecision 123.456 ~digits:2)));
  ("toExponentialWithPrecision - digits:4", (fun _ ->
    Eq("1.2346e+2", toExponentialWithPrecision 123.456 ~digits:4)));
  ("toExponentialWithPrecision - digits:20", (fun _ ->
    Eq("0.00000000000000000000e+0", toExponentialWithPrecision 0. ~digits:20)));
  (__LOC__, (fun _ ->
    ThrowAny(fun () -> ignore @@ toExponentialWithPrecision 0. ~digits:101)));
  ("toExponentialWithPrecision - digits:-1", (fun _ ->
    ThrowAny(fun () -> ignore @@ toExponentialWithPrecision 0. ~digits:(-1))));

  ("toFixed", (fun _ ->
    Eq("123", toFixed 123.456)));
  ("toFixed - large number", (fun _ ->
    Eq("1.2e+21", toFixed 1.2e21)));
  ("toFixedWithPrecision - digits:2", (fun _ ->
    Eq("123.46", toFixedWithPrecision 123.456 ~digits:2)));
  ("toFixedWithPrecision - digits:4", (fun _ ->
    Eq("123.4560", toFixedWithPrecision 123.456 ~digits:4)));
  ("toFixedWithPrecision - digits:20", (fun _ ->
    Eq("0.00000000000000000000", toFixedWithPrecision 0. ~digits:20)));
  ("toFixedWithPrecision - digits:101", (fun _ ->
    ThrowAny(fun () -> ignore @@ toFixedWithPrecision 0. ~digits:101)));
  ("toFixedWithPrecision - digits:-1", (fun _ ->
    ThrowAny(fun () -> ignore @@ toFixedWithPrecision 0. ~digits:(-1))));

  ("toPrecision", (fun _ ->
    Eq("123.456", toPrecision 123.456)));
  ("toPrecision - large number", (fun _ ->
    Eq("1.2e+21", toPrecision 1.2e21)));
  ("toPrecisionWithPrecision - digits:2", (fun _ ->
    Eq("1.2e+2", toPrecisionWithPrecision 123.456 ~digits:2)));
  ("toPrecisionWithPrecision - digits:4", (fun _ ->
    Eq("123.5", toPrecisionWithPrecision 123.456 ~digits:4)));
  ("toPrecisionWithPrecision - digits:20", (fun _ ->
    Eq("0.0000000000000000000", toPrecisionWithPrecision 0. ~digits:20)));
  (__LOC__, (fun _ ->
    ThrowAny(fun () -> ignore @@ toPrecisionWithPrecision 0. ~digits:101)));
  ("toPrecisionWithPrecision - digits:-1", (fun _ ->
    ThrowAny(fun () -> ignore @@ toPrecisionWithPrecision 0. ~digits:(-1))));

  ("toString", (fun _ ->
    Eq("1.23", toString 1.23)));
  ("toString - large number", (fun _ ->
    Eq("1.2e+21", toString 1.2e21)));
  ("toStringWithRadix - radix:2", (fun _ ->
    Eq( "1111011.0111010010111100011010100111111011111001110111",
        toStringWithRadix 123.456 ~radix:2)));
  ("toStringWithRadix - radix:16", (fun _ ->
    Eq("7b.74bc6a7ef9dc", toStringWithRadix 123.456 ~radix:16)));
  ("toStringWithRadix - radix:36", (fun _ ->
    Eq("3f", toStringWithRadix 123. ~radix:36)));
  ("toStringWithRadix - radix:37", (fun _ ->
    ThrowAny(fun () -> ignore @@ toStringWithRadix 0. ~radix:37)));
  ("toStringWithRadix - radix:1", (fun _ ->
    ThrowAny(fun () -> ignore @@ toStringWithRadix 0. ~radix:1)));
  ("toStringWithRadix - radix:-1", (fun _ ->
    ThrowAny(fun () -> ignore @@ toStringWithRadix 0. ~radix:(-1))));

  ("fromString - 123", (fun _ ->
    Eq(123., fromString "123")));
  ("fromString - 12.3", (fun _ ->
    Eq(12.3, fromString "12.3")));
  ("fromString - empty string", (fun _ ->
    Eq(0., fromString "")));
  ("fromString - 0x11", (fun _ ->
    Eq(17., fromString "0x11")));
  ("fromString - 0b11", (fun _ ->
    Eq(3., fromString "0b11")));
  ("fromString - 0o11", (fun _ ->
    Eq(9., fromString "0o11")));
  ("fromString - invalid string", (fun _ ->
    Eq(true, fromString "foo" |> isNaN)));
]

;; Mt.from_pair_suites __FILE__ suites
