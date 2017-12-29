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

eq("File \"ffi_array_test.ml\", line 12, characters 5-12", /* int array */[
        1,
        2,
        3,
        4
      ].map((function (x) {
            return x + 1 | 0;
          })), /* int array */[
      2,
      3,
      4,
      5
    ]);

Mt.from_pair_suites("ffi_array_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
