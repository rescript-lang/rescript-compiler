'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
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

function f() {
  return /* () */0;
}

function ff(x) {
  console.log(x);
  return /* () */0;
}

eq("File \"ignore_test.ml\", line 16, characters 5-12", /* () */0, /* () */0);

Mt.from_pair_suites("ignore_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.ff = ff;
/*  Not a pure module */
