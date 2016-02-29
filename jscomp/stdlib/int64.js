// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_primitive = require("../runtime/caml_primitive");

function succ(n) {
  return n + /* int64 */[
          1,
          0
        ] | 0;
}

function pred(n) {
  return n - /* int64 */[
          1,
          0
        ] | 0;
}

function abs(n) {
  if (n >= /* int64 */[
      0,
      0
    ]) {
    return n;
  }
  else {
    return -n | 0;
  }
}

function lognot(n) {
  return n ^ /* int64 */[
          -1,
          -1
        ];
}

function to_string(n) {
  return Caml_primitive.caml_int64_format("%d", n);
}

function compare(x, y) {
  return Caml_primitive.caml_int64_compare(x, y);
}

var zero = /* int64 */[
  0,
  0
];

var one = /* int64 */[
  1,
  0
];

var minus_one = /* int64 */[
  -1,
  -1
];

var max_int = /* int64 */[
  -1,
  2147483647
];

var min_int = /* int64 */[
  0,
  -2147483648
];

exports.zero      = zero;
exports.one       = one;
exports.minus_one = minus_one;
exports.succ      = succ;
exports.pred      = pred;
exports.abs       = abs;
exports.max_int   = max_int;
exports.min_int   = min_int;
exports.lognot    = lognot;
exports.to_string = to_string;
exports.compare   = compare;
/* No side effect */
