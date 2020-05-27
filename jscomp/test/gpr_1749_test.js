'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

eq("File \"gpr_1749_test.ml\", line 18, characters 6-13", 0, 0);

Mt.from_pair_suites("Gpr_1749_test", suites.contents);

var a = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.a = a;
/*  Not a pure module */
