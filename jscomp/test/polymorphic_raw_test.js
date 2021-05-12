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

var f = ((a) => typeof a);

var a = f(3);

var b = f("3");

eq("File \"polymorphic_raw_test.ml\", line 22, characters 6-13", a, "number");

eq("File \"polymorphic_raw_test.ml\", line 23, characters 6-13", b, "string");

Mt.from_pair_suites("polymorphic_raw_test.ml", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.a = a;
exports.b = b;
/* a Not a pure module */
