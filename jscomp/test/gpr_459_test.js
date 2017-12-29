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

var uu = {
  "'x": 3
};

var uu2 = {
  then: 1,
  catch: 2,
  "'x": 3
};

var hh = uu["'x"];

eq("File \"gpr_459_test.ml\", line 25, characters 12-19", hh, 3);

eq("File \"gpr_459_test.ml\", line 28, characters 5-12", /* tuple */[
      1,
      2,
      3
    ], /* tuple */[
      uu2.then,
      uu2.catch,
      uu2["'x"]
    ]);

Mt.from_pair_suites("gpr_459_test.ml", suites[0]);

/* hh Not a pure module */
