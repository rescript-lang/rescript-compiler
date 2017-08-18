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

function f(x, y) {
  console.log("no inline");
  return x + y | 0;
}

eq("File \"js_nullable_test.ml\", line 13, characters 7-14", /* false */0, /* false */0);

eq("File \"js_nullable_test.ml\", line 15, characters 7-14", +(f(1, 2) == null), /* false */0);

eq("File \"js_nullable_test.ml\", line 17, characters 6-13", +((null) == null), /* true */1);

eq("File \"js_nullable_test.ml\", line 21, characters 3-10", /* false */0, /* false */0);

Mt.from_pair_suites("js_nullable_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.f       = f;
/*  Not a pure module */
