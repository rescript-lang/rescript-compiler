// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64  = require("../runtime/caml_int64");
var Caml_format = require("../runtime/caml_format");

function succ(n) {
  return Caml_int64.add(n, /* int64 */[
              0,
              (1 >>> 0)
            ]);
}

function pred(n) {
  return Caml_int64.sub(n, /* int64 */[
              0,
              (1 >>> 0)
            ]);
}

function abs(n) {
  if (Caml_int64.ge(n, /* int64 */[
          0,
          (0 >>> 0)
        ])) {
    return n;
  }
  else {
    return Caml_int64.neg(n);
  }
}

function lognot(n) {
  return /* int64 */[
          n[0] ^ -1,
          ((n[1] ^ (-1 >>> 0)) >>> 0)
        ];
}

function to_string(n) {
  return Caml_format.caml_int64_format("%d", n);
}

function compare(x, y) {
  return Caml_int64.compare(x, y);
}

var zero_001 = (0 >>> 0);

var zero = /* int64 */[
  0,
  zero_001
];

var one_001 = (1 >>> 0);

var one = /* int64 */[
  0,
  one_001
];

var minus_one_001 = (-1 >>> 0);

var minus_one = /* int64 */[
  -1,
  minus_one_001
];

var max_int_001 = (-1 >>> 0);

var max_int = /* int64 */[
  2147483647,
  max_int_001
];

var min_int_001 = (0 >>> 0);

var min_int = /* int64 */[
  -2147483648,
  min_int_001
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
