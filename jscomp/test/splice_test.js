'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

Math.max(1);

function f00(a, b) {
  return a.send(b);
}

function f1(c) {
  return Block.spliceApply(Math.max, [
              1,
              c
            ]);
}

eq("File \"splice_test.ml\", line 22, characters 6-13", Math.max(1, 2, 3), 3);

eq("File \"splice_test.ml\", line 23, characters 6-13", Math.max(1), 1);

eq("File \"splice_test.ml\", line 24, characters 6-13", Math.max(1, 1, 2, 3, 4, 5, 2, 3), 5);

Mt.from_pair_suites("splice_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f00 = f00;
exports.f1 = f1;
/*  Not a pure module */
