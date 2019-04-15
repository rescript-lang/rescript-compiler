'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function foo(a){return a()}
;

function fn(param) {
  return 1;
}

eq("File \"gpr_3492_test.ml\", line 12, characters 6-13", foo((function () {
            return Curry._1(fn(/* () */0), /* () */0);
          })), 1);

Mt.from_pair_suites("gpr_3492_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.fn = fn;
/*  Not a pure module */
