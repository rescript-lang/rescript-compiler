'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
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

(function (n) {
      return 0;
    })((function (q, y) {
        return false;
      })) === 0;

eq("File \"gpr_1667_test.ml\", line 18, characters 7-14", 0, 0);

Mt.from_pair_suites("gpr_1667_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
