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

function make(x) {
  return x;
}

function get(x) {
  return x;
}

var x = "foo";

eq("File \"unboxed_attribute_test.ml\", line 18, characters 3-10", x, x);

var x$1 = "foo";

eq("File \"unboxed_attribute_test.ml\", line 26, characters 3-10", x$1, x$1);

var x$2 = "foo";

eq("File \"unboxed_attribute_test.ml\", line 33, characters 3-10", x$2, x$2);

var y = [];

y[0] = y;

Mt.from_pair_suites("unboxed_attribute_test.ml", suites.contents);

var v0 = 3;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v0 = v0;
exports.make = make;
exports.get = get;
exports.y = y;
/*  Not a pure module */
