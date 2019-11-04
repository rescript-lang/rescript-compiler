'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Int32 = require("../../lib/js/int32.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = [/* contents : [] */0];

var test_id = [/* contents */0];

function eq(loc, x, y) {
  test_id[/* contents */0] = test_id[/* contents */0] + 1 | 0;
  suites[/* contents */0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[/* contents */0])),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[/* contents */0]
  ];
  return /* () */0;
}

eq("File \"limits_test.ml\", line 10, characters 5-12", Pervasives.max_int, (2147483647));

eq("File \"limits_test.ml\", line 11, characters 5-12", Pervasives.min_int, (-2147483648));

eq("File \"limits_test.ml\", line 12, characters 5-12", Int32.max_int, (2147483647));

eq("File \"limits_test.ml\", line 13, characters 5-12", Int32.min_int, (-2147483648));

Mt.from_pair_suites("Limits_test", suites[/* contents */0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
