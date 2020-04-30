'use strict';

var Mt = require("./mt.js");
var Format = require("../../lib/js/format.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var v = {
  a: 3,
  b: {
    xx: 2,
    yy: 3
  }
};

var u_a = 2;

var u_b = {
  xx: 2,
  yy: 3
};

var u = {
  a: u_a,
  b: u_b
};

var A = Caml_exceptions.create("Record_debug_test.A");

var B = Caml_exceptions.create("Record_debug_test.B");

var v0 = {
  ExceptionID: A,
  _1: 3
};

var v1 = {
  ExceptionID: B,
  _1: 3,
  _2: 2
};

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

console.log(" hei " + v + " ");

var a = /* tuple */[
  1,
  2,
  2,
  4,
  3
];

var c = [
  1,
  2,
  3,
  4,
  5
];

console.log(" " + Format.std_formatter + " " + a + " " + c + " ");

var a_000 = "";

var a_001 = "a";

var a_002 = "" + 3;

var a_003 = "" + 3 + 3;

var a_004 = "" + 3 + 3 + 3;

var a_005 = " " + 3;

var a$1 = /* tuple */[
  a_000,
  a_001,
  a_002,
  a_003,
  a_004,
  a_005
];

eq("File \"record_debug_test.ml\", line 64, characters 3-10", a$1, /* tuple */[
      "",
      "a",
      "3",
      "33",
      "333",
      " 3"
    ]);

Mt.from_pair_suites("record_debug_test.ml", suites.contents);

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

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
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
