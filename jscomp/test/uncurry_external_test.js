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

function sum(a,b){
  return a + b
}
;

var h = sum(1.0, 2.0);

eq("File \"uncurry_external_test.ml\", line 25, characters 5-12", h, 3);

Mt.from_pair_suites("Uncurry_external_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.h = h;
/*  Not a pure module */
