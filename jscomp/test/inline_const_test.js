'use strict';

var Mt = require("./mt.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var H = /* module */[];

var f = "hello";

var f1 = "a";

var f2 = "中文";

var f3 = "中文";

var f4 = "中文";

eq("File \"inline_const_test.ml\", line 23, characters 5-12", f, "hello");

eq("File \"inline_const_test.ml\", line 24, characters 5-12", f1, "a");

eq("File \"inline_const_test.ml\", line 25, characters 5-12", f2, "中文");

eq("File \"inline_const_test.ml\", line 26, characters 5-12", f3, "中文");

eq("File \"inline_const_test.ml\", line 27, characters 5-12", f4, "中文");

Mt.from_pair_suites("File \"inline_const_test.ml\", line 30, characters 22-29", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.H = H;
exports.f = f;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
/*  Not a pure module */
