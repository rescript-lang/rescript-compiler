'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

var a = /* float array */[
  0,1,2
];

var b = /* int array */[
  0,1,2
];

var c = /* array */[
  0,1,2,3,4,5
];

function f() {
  a[0] = 3.0;
  b[0] = 3;
  return /* () */0;
}

function h() {
  return c;
}

function g() {
  f(/* () */0);
  return /* Eq */Block.__(0, [
            /* tuple */[
              a[0],b[0]
            ],/* tuple */[
              3.0,3
            ]
          ]);
}

var suites_000 = /* tuple */[
  "const_block_test",g
];

var suites_001 = /* Nested :: */[
  /* tuple */[
    "avoid_mutable_inline_test",function () {
      c[0] = 3;
      c[1] = 4;
      return /* Eq */Block.__(0, [
                /* array */[
                  3,4,2,3,4,5
                ],c
              ]);
    }
  ],/* [] */0
];

var suites = /* Nested :: */[
  suites_000,suites_001
];

Mt.from_pair_suites("const_block_test.ml", suites);

var v = /* tuple */[
  0,1,2,3,4,5
];

exports.a = a;
exports.b = b;
exports.c = c;
exports.v = v;
exports.f = f;
exports.h = h;
/*  Not a pure module */
