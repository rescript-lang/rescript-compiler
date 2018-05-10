'use strict';

var Mt = require("./mt.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_SetInt = require("../../lib/js/belt_SetInt.js");
var Array_data_util = require("./array_data_util.js");
var Belt_HashSetInt = require("../../lib/js/belt_HashSetInt.js");
var Belt_SortArrayInt = require("../../lib/js/belt_SortArrayInt.js");
var Belt_internalBucketsType = require("../../lib/js/belt_internalBucketsType.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function add(x, y) {
  return x + y | 0;
}

function sum2(h) {
  var v = [0];
  Belt_HashSetInt.forEach(h, (function (x) {
          v[0] = v[0] + x | 0;
          return /* () */0;
        }));
  return v[0];
}

var u = Belt_Array.concat(Array_data_util.randomRange(30, 100), Array_data_util.randomRange(40, 120));

var v = Belt_HashSetInt.ofArray(u);

eq("File \"bs_hashset_int_test.ml\", line 19, characters 5-12", v.size, 91);

var xs = Belt_SetInt.toArray(Belt_SetInt.ofArray(Belt_HashSetInt.toArray(v)));

eq("File \"bs_hashset_int_test.ml\", line 21, characters 5-12", xs, Array_data_util.range(30, 120));

eq("File \"bs_hashset_int_test.ml\", line 23, characters 5-12", Belt_HashSetInt.reduce(v, 0, add), 6825);

eq("File \"bs_hashset_int_test.ml\", line 24, characters 5-12", sum2(v), 6825);

var u$1 = Belt_Array.concat(Array_data_util.randomRange(0, 100000), Array_data_util.randomRange(0, 100));

var v$1 = Belt_internalBucketsType.make(/* () */0, /* () */0, 40);

Belt_HashSetInt.mergeMany(v$1, u$1);

eq("File \"bs_hashset_int_test.ml\", line 30, characters 5-12", v$1.size, 100001);

for(var i = 0; i <= 1000; ++i){
  Belt_HashSetInt.remove(v$1, i);
}

eq("File \"bs_hashset_int_test.ml\", line 34, characters 5-12", v$1.size, 99000);

for(var i$1 = 0; i$1 <= 2000; ++i$1){
  Belt_HashSetInt.remove(v$1, i$1);
}

eq("File \"bs_hashset_int_test.ml\", line 38, characters 5-12", v$1.size, 98000);

var u0 = Belt_HashSetInt.ofArray(Array_data_util.randomRange(0, 100000));

var u1 = Belt_HashSetInt.copy(u0);

eq("File \"bs_hashset_int_test.ml\", line 46, characters 5-12", Belt_HashSetInt.toArray(u0), Belt_HashSetInt.toArray(u1));

for(var i$2 = 0; i$2 <= 2000; ++i$2){
  Belt_HashSetInt.remove(u1, i$2);
}

for(var i$3 = 0; i$3 <= 1000; ++i$3){
  Belt_HashSetInt.remove(u0, i$3);
}

var v0 = Belt_Array.concat(Array_data_util.range(0, 1000), Belt_HashSetInt.toArray(u0));

var v1 = Belt_Array.concat(Array_data_util.range(0, 2000), Belt_HashSetInt.toArray(u1));

Belt_SortArrayInt.stableSortInPlace(v0);

Belt_SortArrayInt.stableSortInPlace(v1);

eq("File \"bs_hashset_int_test.ml\", line 57, characters 5-12", v0, v1);

var h = Belt_HashSetInt.ofArray(Array_data_util.randomRange(0, 1000000));

var histo = Belt_HashSetInt.getBucketHistogram(h);

b("File \"bs_hashset_int_test.ml\", line 62, characters 4-11", histo.length <= 10);

Mt.from_pair_suites("bs_hashset_int_test.ml", suites[0]);

var N = 0;

var S = 0;

var I = 0;

var $plus$plus = Belt_Array.concat;

var A = 0;

var SI = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.S = S;
exports.I = I;
exports.$plus$plus = $plus$plus;
exports.add = add;
exports.sum2 = sum2;
exports.A = A;
exports.SI = SI;
/* u Not a pure module */
