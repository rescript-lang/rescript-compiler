'use strict';

var Mt              = require("./mt.js");
var Bs_Sort         = require("../../lib/js/bs_Sort.js");
var Bs_Array        = require("../../lib/js/bs_Array.js");
var Bs_Range        = require("../../lib/js/bs_Range.js");
var Array_data_util = require("./array_data_util.js");

var suites = [/* [] */0];

var test_id = [0];

function eqx(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function cmp(x, y) {
  return x - y | 0;
}

b("File \"bs_sort_test.ml\", line 11, characters 4-11", Bs_Range.forAll(0, 200, (function (i) {
            var v = Array_data_util.randomRange(0, i);
            Bs_Sort.stableSortBy(v, cmp);
            return Bs_Sort.isSorted(v, cmp);
          })));

b("File \"bs_sort_test.ml\", line 17, characters 4-11", Bs_Range.forAll(0, 200, (function (i) {
            var v = Array_data_util.randomRange(0, i);
            v.sort(cmp);
            return Bs_Sort.isSorted(v, cmp);
          })));

b("File \"bs_sort_test.ml\", line 23, characters 4-11", Bs_Sort.isSorted(/* int array */[], cmp));

b("File \"bs_sort_test.ml\", line 26, characters 4-11", Bs_Sort.isSorted(/* int array */[0], cmp));

b("File \"bs_sort_test.ml\", line 29, characters 4-11", Bs_Sort.isSorted(/* int array */[
          0,
          1
        ], cmp));

b("File \"bs_sort_test.ml\", line 31, characters 4-11", 1 - Bs_Sort.isSorted(/* int array */[
          1,
          0
        ], cmp));

var u = Array_data_util.randomRange(0, 1000000);

var u1 = Bs_Array.copy(u);

console.time("bs_sort_test.ml 38");

Bs_Sort.stableSortBy(u, cmp);

console.timeEnd("bs_sort_test.ml 38");

b("File \"bs_sort_test.ml\", line 39, characters 4-11", Bs_Sort.isSorted(u, cmp));

console.time("bs_sort_test.ml 40");

u1.sort(cmp);

console.timeEnd("bs_sort_test.ml 40");

b("File \"bs_sort_test.ml\", line 41, characters 4-11", Bs_Sort.isSorted(u1, cmp));

Mt.from_pair_suites("bs_sort_test.ml", suites[0]);

var I = 0;

var S = 0;

var R = 0;

var A = 0;

exports.suites  = suites;
exports.test_id = test_id;
exports.eqx     = eqx;
exports.b       = b;
exports.I       = I;
exports.S       = S;
exports.R       = R;
exports.cmp     = cmp;
exports.A       = A;
/*  Not a pure module */
