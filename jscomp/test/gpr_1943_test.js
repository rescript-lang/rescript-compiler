'use strict';

var Mt    = require("./mt.js");
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

function f(x) {
  return /* tuple */[
          x["003"],
          x["50"],
          x["50x"],
          x.__50,
          x.__50x,
          x["50x'"],
          x["x'"]
        ];
}

var v = f({
      "003": 0,
      "50": 1,
      "50x": 2,
      __50: 3,
      __50x: 4,
      "50x'": 5,
      "x'": 6
    });

eq("File \"gpr_1943_test.ml\", line 30, characters 6-13", /* tuple */[
      0,
      1,
      2,
      3,
      4,
      5,
      6
    ], v);

Mt.from_pair_suites("gpr_1943_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.f       = f;
exports.v       = v;
/* v Not a pure module */
