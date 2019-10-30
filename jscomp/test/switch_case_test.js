'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

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

function f(x) {
  switch (x) {
    case "xx\"" :
        return 1;
    case "xx'''" :
        return 0;
    case "xx\\\"" :
        return 2;
    case "xx\\\"\"" :
        return 3;
    default:
      return 4;
  }
}

eq("File \"switch_case_test.ml\", line 19, characters 7-14", f("xx'''"), 0);

eq("File \"switch_case_test.ml\", line 20, characters 7-14", f("xx\""), 1);

eq("File \"switch_case_test.ml\", line 21, characters 7-14", f("xx\\\""), 2);

eq("File \"switch_case_test.ml\", line 22, characters 7-14", f("xx\\\"\""), 3);

Mt.from_pair_suites("Switch_case_test", suites[/* contents */0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
/*  Not a pure module */
