'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

function ok(loc, x) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Ok */Block.__(2, [x]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

var match = typeof (___undefined_value) === "undefined" ? undefined : (___undefined_value);

var a = match !== undefined ? 2 : 1;

function f(x) {
  return +(x === undefined);
}

ok('File "undef_regression2_test.ml", line 27, characters 5-12', +(a > 0));

eq('File "undef_regression2_test.ml", line 28, characters 5-12', a, 1);

Mt.from_pair_suites("undef_regression2_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.ok      = ok;
exports.a       = a;
exports.f       = f;
/* match Not a pure module */
