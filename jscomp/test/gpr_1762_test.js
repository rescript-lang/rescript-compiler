'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

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

var v = [/* contents */3];

function update(param) {
  v[/* contents */0] = v[/* contents */0] + 1 | 0;
  return true;
}

v[/* contents */0] = v[/* contents */0] + 1 | 0;

eq("File \"gpr_1762_test.ml\", line 22, characters 6-13", v[/* contents */0], 4);

Mt.from_pair_suites("Gpr_1762_test", suites[/* contents */0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.update = update;
/*  Not a pure module */
