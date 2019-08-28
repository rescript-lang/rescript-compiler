'use strict';

var Format = require("../../lib/js/format.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var v = /* record */{
  a: 3,
  b: /* record */{
    xx: 2,
    yy: 3
  }
};

var u = /* record */{
  a: 2,
  b: /* record */{
    xx: 2,
    yy: 3
  }
};

var A = Caml_exceptions.create("Record_debug_test.A");

var B = Caml_exceptions.create("Record_debug_test.B");

var v0 = [
  A,
  3
];

var v1 = [
  B,
  3,
  2
];

var N = {
  a: 0,
  b: 1
};

function N0_f(prim) {
  return prim;
}

var N0 = {
  a: 0,
  b: 1,
  f: N0_f
};

console.log(" hei " + (String(v) + " "));

var a = /* tuple */[
  1,
  2,
  2,
  4,
  3
];

var c = /* array */[
  1,
  2,
  3,
  4,
  5
];

console.log(" " + (String(Format.std_formatter) + (" " + (String(a) + (" " + (String(c) + " "))))));

var h = /* :: */[
  1,
  /* :: */[
    2,
    /* :: */[
      3,
      /* :: */[
        4,
        /* [] */0
      ]
    ]
  ]
];

var v2 = /* `C */[
  67,
  2
];

var v3 = /* `C */[
  67,
  /* tuple */[
    2,
    3
  ]
];

var fmt = Format.std_formatter;

exports.v = v;
exports.u = u;
exports.h = h;
exports.A = A;
exports.B = B;
exports.v0 = v0;
exports.v1 = v1;
exports.v2 = v2;
exports.v3 = v3;
exports.N = N;
exports.N0 = N0;
exports.fmt = fmt;
exports.a = a;
exports.c = c;
/*  Not a pure module */
