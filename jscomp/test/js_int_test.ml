open Js_int

let suites = Mt.[
  ("toExponential", (fun _ ->
    Eq("1.23456e+5", toExponential 123456)));
  ("toExponentialWithPrecision - digits:2", (fun _ ->
    Eq("1.23e+5", toExponentialWithPrecision 123456 ~digits:2)));
  ("toExponentialWithPrecision - digits:4", (fun _ ->
    Eq("1.2346e+5", toExponentialWithPrecision 123456 ~digits:4)));
  ("toExponentialWithPrecision - digits:20", (fun _ ->
    Eq("0.00000000000000000000e+0", toExponentialWithPrecision 0 ~digits:20)));
  (__LOC__, (fun _ ->
    ThrowAny(fun () -> ignore @@ toExponentialWithPrecision 0 ~digits:101)));
  ("toExponentialWithPrecision - digits:-1", (fun _ ->
    ThrowAny(fun () -> ignore @@ toExponentialWithPrecision 0 ~digits:(-1))));

  ("toPrecision", (fun _ ->
    Eq("123456", toPrecision 123456)));
  ("toPrecisionWithPrecision - digits:2", (fun _ ->
    Eq("1.2e+5", toPrecisionWithPrecision 123456 ~digits:2)));
  ("toPrecisionWithPrecision - digits:4", (fun _ ->
    Eq("1.235e+5", toPrecisionWithPrecision 123456 ~digits:4)));
  ("toPrecisionWithPrecision - digits:20", (fun _ ->
    Eq("0.0000000000000000000", toPrecisionWithPrecision 0 ~digits:20)));
  (__LOC__, (fun _ ->
    ThrowAny(fun () -> ignore @@ toPrecisionWithPrecision 0 ~digits:101)));
  ("toPrecisionWithPrecision - digits:-1", (fun _ ->
    ThrowAny(fun () -> ignore @@ toPrecisionWithPrecision 0 ~digits:(-1))));
    
  ("toString", (fun _ ->
    Eq("123", toString 123)));
  ("toStringWithRadix - radix:2", (fun _ ->
    Eq("11110001001000000", toStringWithRadix 123456 ~radix:2)));
  ("toStringWithRadix - radix:16", (fun _ ->
    Eq("1e240", toStringWithRadix 123456 ~radix:16)));
  ("toStringWithRadix - radix:36", (fun _ ->
    Eq("2n9c", toStringWithRadix 123456 ~radix:36)));
  ("toStringWithRadix - radix:37", (fun _ ->
    ThrowAny(fun () -> ignore @@ toStringWithRadix 0 ~radix:37)));
  ("toStringWithRadix - radix:1", (fun _ ->
    ThrowAny(fun () -> ignore @@ toStringWithRadix 0 ~radix:1)));
  ("toStringWithRadix - radix:-1", (fun _ ->
    ThrowAny(fun () -> ignore @@ toStringWithRadix 0 ~radix:(-1))));
]

;; Mt.from_pair_suites __FILE__ suites
