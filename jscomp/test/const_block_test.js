// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Mt = require("./mt");

var a = /* array */[
  0,
  1,
  2
];

var b = /* array */[
  0,
  1,
  2
];

var c = /* array */[
  0,
  1,
  2,
  3,
  4,
  5
];

function f() {
  a[0] = 3;
  b[0] = 3;
  return /* () */0;
}

function h() {
  return c;
}

function g() {
  f(/* () */0);
  return [
          /* Eq */0,
          [
            /* tuple */0,
            a[0],
            b[0]
          ],
          [
            /* tuple */0,
            3,
            3
          ]
        ];
}

var suites_001 = [
  /* tuple */0,
  "const_block_test",
  g
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "avoid_mutable_inline_test",
    function () {
      var v = h(/* () */0);
      var v2 = h(/* () */0);
      v[0] = 3;
      v2[1] = 4;
      return [
              /* Eq */0,
              /* array */[
                3,
                4,
                2,
                3,
                4,
                5
              ],
              v
            ];
    }
  ],
  /* [] */0
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("const_block_test.ml", suites);

var v = [
  /* tuple */0,
  0,
  1,
  2,
  3,
  4,
  5
];

exports.a = a;
exports.b = b;
exports.c = c;
exports.v = v;
exports.f = f;
exports.h = h;
/*  fail the pure module */
