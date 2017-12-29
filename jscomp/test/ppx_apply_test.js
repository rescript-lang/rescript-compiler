'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

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

var u = 3;

function nullary() {
  return 3;
}

function unary(a) {
  return a + 3 | 0;
}

var xx = unary(3);

eq("File \"ppx_apply_test.ml\", line 17, characters 5-12", u, 3);

Mt.from_pair_suites("ppx_apply_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
exports.nullary = nullary;
exports.unary = unary;
exports.xx = xx;
/* u Not a pure module */
