'use strict';

var Mt       = require("./mt.js");
var Bs_Array = require("../../lib/js/bs_Array.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function add(x, y) {
  return x + y | 0;
}

Mt.from_pair_suites("bs_hashmap_test.ml", suites[0]);

var N = 0;

var S = 0;

var I = 0;

var $plus$plus = Bs_Array.append;

exports.suites     = suites;
exports.test_id    = test_id;
exports.eq         = eq;
exports.N          = N;
exports.S          = S;
exports.I          = I;
exports.$plus$plus = $plus$plus;
exports.add        = add;
/*  Not a pure module */
