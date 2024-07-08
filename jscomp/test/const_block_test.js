// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml_array = require("../../lib/js/caml_array.js");

let a = [
  0,
  1,
  2
];

let b = [
  0,
  1,
  2
];

let c = [
  0,
  1,
  2,
  3,
  4,
  5
];

function f() {
  Caml_array.set(a, 0, 3.0);
  Caml_array.set(b, 0, 3);
}

function h() {
  return c;
}

function g() {
  f();
  return {
    TAG: "Eq",
    _0: [
      Caml_array.get(a, 0),
      Caml_array.get(b, 0)
    ],
    _1: [
      3.0,
      3
    ]
  };
}

let suites_0 = [
  "const_block_test",
  g
];

let suites_1 = {
  hd: [
    "avoid_mutable_inline_test",
    (function () {
      Caml_array.set(c, 0, 3);
      Caml_array.set(c, 1, 4);
      return {
        TAG: "Eq",
        _0: [
          3,
          4,
          2,
          3,
          4,
          5
        ],
        _1: c
      };
    })
  ],
  tl: /* [] */0
};

let suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Const_block_test", suites);

let v = [
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
/*  Not a pure module */
