'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function f(xOpt, y) {
  var x = xOpt !== void 0 ? xOpt : 3;
  var xOpt$1 = x + 2 | 0;
  console.log(xOpt$1);
  return xOpt$1 + y | 0;
}

console.log(f(void 0, 2));

eq("File \"test_case_opt_collision.ml\", line 15, characters 6-13", f(void 0, 2), 7);

eq("File \"test_case_opt_collision.ml\", line 17, characters 6-13", f(4, 2), 8);

Mt.from_pair_suites("test_case_opt_collision.ml", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
/*  Not a pure module */
