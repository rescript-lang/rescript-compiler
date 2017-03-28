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

function nullary() {
  return 3;
}

function unary(a) {
  return a + 3 | 0;
}

eq('File "ppx_apply_test.ml", line 17, characters 5-12', 3, 3);

Mt.from_pair_suites("ppx_apply_test.ml", suites[0]);

var u = 3;

var xx = 6;

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.u       = u;
exports.nullary = nullary;
exports.unary   = unary;
exports.xx      = xx;
/*  Not a pure module */
