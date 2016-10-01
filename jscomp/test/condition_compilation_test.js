'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

var v = [1];

v[0] = v[0] + 1 | 0;

var a = v[0];

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

eq('File "condition_compilation_test.ml", line 28, characters 5-12', 3, 3);

eq('File "condition_compilation_test.ml", line 29, characters 5-12', v[0], 4);

Mt.from_pair_suites("condition_compilation_test.ml", suites[0]);

var vv = 3;

exports.vv      = vv;
exports.v       = v;
exports.a       = a;
exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
