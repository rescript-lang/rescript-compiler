'use strict';

var Mt        = require("./mt.js");
var $$Array   = require("../../lib/js/array.js");
var Block     = require("../../lib/js/block.js");
var Bs_SetInt = require("../../lib/js/bs_SetInt.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function b(loc, v) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Ok */Block.__(4, [v]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function $eq$tilde(s, i) {
  return Bs_SetInt.eq(Bs_SetInt.ofArray(i), s);
}

function $eq$star(a, b) {
  return Bs_SetInt.eq(Bs_SetInt.ofArray(a), Bs_SetInt.ofArray(b));
}

b("File \"bs_set_int_test.ml\", line 24, characters 4-11", $eq$star(/* int array */[
          1,
          2,
          3
        ], /* int array */[
          3,
          2,
          1
        ]));

var u = Bs_SetInt.inter(Bs_SetInt.ofArray(/* int array */[
          1,
          2,
          3
        ]), Bs_SetInt.ofArray(/* int array */[
          3,
          4,
          5
        ]));

b("File \"bs_set_int_test.ml\", line 30, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(/* int array */[3]), u));

function range(i, j) {
  return $$Array.init((j - i | 0) + 1 | 0, (function (k) {
                return k + i | 0;
              }));
}

var i = range(50, 100);

var s = Bs_SetInt.inter(Bs_SetInt.ofArray(range(1, 100)), Bs_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 39, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i), s));

var i$1 = range(1, 200);

var s$1 = Bs_SetInt.union(Bs_SetInt.ofArray(range(1, 100)), Bs_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 42, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i$1), s$1));

var i$2 = range(1, 49);

var s$2 = Bs_SetInt.diff(Bs_SetInt.ofArray(range(1, 100)), Bs_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 45, characters 6-13", Bs_SetInt.eq(Bs_SetInt.ofArray(i$2), s$2));

Mt.from_pair_suites("bs_set_int_test.ml", suites[0]);

var N = 0;

var ofA = Bs_SetInt.ofArray;

exports.suites    = suites;
exports.test_id   = test_id;
exports.eq        = eq;
exports.b         = b;
exports.N         = N;
exports.$eq$tilde = $eq$tilde;
exports.$eq$star  = $eq$star;
exports.ofA       = ofA;
exports.u         = u;
exports.range     = range;
/*  Not a pure module */
