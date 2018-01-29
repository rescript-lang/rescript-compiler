'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_SetInt = require("../../lib/js/bs_SetInt.js");
var Array_data_util = require("./array_data_util.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, v) {
  return Mt.bool_suites(test_id, suites, loc, v);
}

function $eq$tilde(s, i) {
  return Bs_SetInt.eq(Bs_SetInt.ofArray(i), s);
}

function $eq$star(a, b) {
  return Bs_SetInt.eq(Bs_SetInt.ofArray(a), Bs_SetInt.ofArray(b));
}

b("File \"bs_set_int_test.ml\", line 17, characters 4-11", $eq$star(/* int array */[
          1,
          2,
          3
        ], /* int array */[
          3,
          2,
          1
        ]));

var u = Bs_SetInt.intersect(Bs_SetInt.ofArray(/* int array */[
          1,
          2,
          3
        ]), Bs_SetInt.ofArray(/* int array */[
          3,
          4,
          5
        ]));

b("File \"bs_set_int_test.ml\", line 23, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(/* int array */[3]), u));

function range(i, j) {
  return $$Array.init((j - i | 0) + 1 | 0, (function (k) {
                return k + i | 0;
              }));
}

function revRange(i, j) {
  return $$Array.of_list(List.rev($$Array.to_list($$Array.init((j - i | 0) + 1 | 0, (function (k) {
                            return k + i | 0;
                          })))));
}

var v = Bs_SetInt.ofArray($$Array.append(range(100, 1000), revRange(400, 1500)));

var i = range(100, 1500);

b("File \"bs_set_int_test.ml\", line 36, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i), v));

var match = Bs_SetInt.partition(v, (function (x) {
        return +(x % 3 === 0);
      }));

var l = Bs_SetInt.empty;

var r = Bs_SetInt.empty;

for(var i$1 = 100; i$1 <= 1500; ++i$1){
  if (i$1 % 3) {
    r = Bs_SetInt.add(r, i$1);
  } else {
    l = Bs_SetInt.add(l, i$1);
  }
}

b("File \"bs_set_int_test.ml\", line 47, characters 4-11", Bs_SetInt.eq(match[0], l));

b("File \"bs_set_int_test.ml\", line 48, characters 4-11", Bs_SetInt.eq(match[1], r));

var i$2 = range(50, 100);

var s = Bs_SetInt.intersect(Bs_SetInt.ofArray(range(1, 100)), Bs_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 51, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i$2), s));

var i$3 = range(1, 200);

var s$1 = Bs_SetInt.union(Bs_SetInt.ofArray(range(1, 100)), Bs_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 54, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i$3), s$1));

var i$4 = range(1, 49);

var s$2 = Bs_SetInt.diff(Bs_SetInt.ofArray(range(1, 100)), Bs_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 57, characters 6-13", Bs_SetInt.eq(Bs_SetInt.ofArray(i$4), s$2));

var i$5 = revRange(50, 100);

var s$3 = Bs_SetInt.intersect(Bs_SetInt.ofArray(revRange(1, 100)), Bs_SetInt.ofArray(revRange(50, 200)));

b("File \"bs_set_int_test.ml\", line 60, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i$5), s$3));

var i$6 = revRange(1, 200);

var s$4 = Bs_SetInt.union(Bs_SetInt.ofArray(revRange(1, 100)), Bs_SetInt.ofArray(revRange(50, 200)));

b("File \"bs_set_int_test.ml\", line 63, characters 4-11", Bs_SetInt.eq(Bs_SetInt.ofArray(i$6), s$4));

var i$7 = revRange(1, 49);

var s$5 = Bs_SetInt.diff(Bs_SetInt.ofArray(revRange(1, 100)), Bs_SetInt.ofArray(revRange(50, 200)));

b("File \"bs_set_int_test.ml\", line 66, characters 6-13", Bs_SetInt.eq(Bs_SetInt.ofArray(i$7), s$5));

var ss = /* array */[
  1,
  222,
  3,
  4,
  2,
  0,
  33,
  -1
];

var v$1 = Bs_SetInt.ofArray(/* array */[
      1,
      222,
      3,
      4,
      2,
      0,
      33,
      -1
    ]);

var minv = Bs_SetInt.minUndefined(v$1);

var maxv = Bs_SetInt.maxUndefined(v$1);

function approx(loc, x, y) {
  return b(loc, +(x === y));
}

eq("File \"bs_set_int_test.ml\", line 74, characters 5-12", Bs_SetInt.reduce(v$1, 0, (function (x, y) {
            return x + y | 0;
          })), Bs_Array.reduce(ss, 0, (function (x, y) {
            return x + y | 0;
          })));

approx("File \"bs_set_int_test.ml\", line 75, characters 9-16", -1, minv);

approx("File \"bs_set_int_test.ml\", line 76, characters 9-16", 222, maxv);

var v$2 = Bs_SetInt.remove(v$1, 3);

var minv$1 = Bs_SetInt.minimum(v$2);

var maxv$1 = Bs_SetInt.maximum(v$2);

eq("File \"bs_set_int_test.ml\", line 79, characters 5-12", minv$1, /* Some */[-1]);

eq("File \"bs_set_int_test.ml\", line 80, characters 5-12", maxv$1, /* Some */[222]);

var v$3 = Bs_SetInt.remove(v$2, 222);

var minv$2 = Bs_SetInt.minimum(v$3);

var maxv$2 = Bs_SetInt.maximum(v$3);

eq("File \"bs_set_int_test.ml\", line 83, characters 5-12", minv$2, /* Some */[-1]);

eq("File \"bs_set_int_test.ml\", line 84, characters 5-12", maxv$2, /* Some */[33]);

var v$4 = Bs_SetInt.remove(v$3, -1);

var minv$3 = Bs_SetInt.minimum(v$4);

var maxv$3 = Bs_SetInt.maximum(v$4);

eq("File \"bs_set_int_test.ml\", line 87, characters 5-12", minv$3, /* Some */[0]);

eq("File \"bs_set_int_test.ml\", line 88, characters 5-12", maxv$3, /* Some */[33]);

var v$5 = Bs_SetInt.remove(v$4, 0);

var v$6 = Bs_SetInt.remove(v$5, 33);

var v$7 = Bs_SetInt.remove(v$6, 2);

var v$8 = Bs_SetInt.remove(v$7, 3);

var v$9 = Bs_SetInt.remove(v$8, 4);

var v$10 = Bs_SetInt.remove(v$9, 1);

b("File \"bs_set_int_test.ml\", line 95, characters 4-11", Bs_SetInt.isEmpty(v$10));

var v$11 = Bs_Array.makeByAndShuffle(1000000, (function (i) {
        return i;
      }));

var u$1 = Bs_SetInt.ofArray(v$11);

b("File \"bs_set_int_test.ml\", line 102, characters 4-11", Bs_SetInt.checkInvariantInternal(u$1));

var firstHalf = Bs_Array.slice(v$11, 0, 2000);

var xx = Bs_Array.reduce(firstHalf, u$1, Bs_SetInt.remove);

b("File \"bs_set_int_test.ml\", line 106, characters 4-11", Bs_SetInt.checkInvariantInternal(u$1));

b("File \"bs_set_int_test.ml\", line 107, characters 4-11", Bs_SetInt.eq(Bs_SetInt.union(Bs_SetInt.ofArray(firstHalf), xx), u$1));

var aa = Bs_SetInt.ofArray(Array_data_util.randomRange(0, 100));

var bb = Bs_SetInt.ofArray(Array_data_util.randomRange(0, 200));

var cc = Bs_SetInt.ofArray(Array_data_util.randomRange(120, 200));

var dd = Bs_SetInt.union(aa, cc);

b("File \"bs_set_int_test.ml\", line 114, characters 4-11", Bs_SetInt.subset(aa, bb));

b("File \"bs_set_int_test.ml\", line 115, characters 4-11", Bs_SetInt.subset(dd, bb));

b("File \"bs_set_int_test.ml\", line 116, characters 4-11", Bs_SetInt.subset(Bs_SetInt.add(dd, 200), bb));

b("File \"bs_set_int_test.ml\", line 117, characters 4-11", +(Bs_SetInt.add(dd, 200) === dd));

b("File \"bs_set_int_test.ml\", line 118, characters 4-11", +(Bs_SetInt.add(dd, 0) === dd));

b("File \"bs_set_int_test.ml\", line 119, characters 4-11", 1 - Bs_SetInt.subset(Bs_SetInt.add(dd, 201), bb));

var aa$1 = Bs_SetInt.ofArray(Array_data_util.randomRange(0, 100));

var bb$1 = Bs_SetInt.ofArray(Array_data_util.randomRange(0, 100));

var cc$1 = Bs_SetInt.add(bb$1, 101);

var dd$1 = Bs_SetInt.remove(bb$1, 99);

var ee = Bs_SetInt.add(dd$1, 101);

b("File \"bs_set_int_test.ml\", line 128, characters 4-11", Bs_SetInt.eq(aa$1, bb$1));

b("File \"bs_set_int_test.ml\", line 129, characters 4-11", 1 - Bs_SetInt.eq(aa$1, cc$1));

b("File \"bs_set_int_test.ml\", line 130, characters 4-11", 1 - Bs_SetInt.eq(dd$1, cc$1));

b("File \"bs_set_int_test.ml\", line 131, characters 4-11", 1 - Bs_SetInt.eq(bb$1, ee));

var a1 = Bs_SetInt.mergeMany(Bs_SetInt.empty, Array_data_util.randomRange(0, 100));

var a2 = Bs_SetInt.removeMany(a1, Array_data_util.randomRange(40, 100));

var a3 = Bs_SetInt.ofArray(Array_data_util.randomRange(0, 39));

var match$1 = Bs_SetInt.split(a1, 40);

var match$2 = match$1[0];

var a5 = match$2[1];

var a4 = match$2[0];

b("File \"bs_set_int_test.ml\", line 139, characters 4-11", Bs_SetInt.eq(a1, Bs_SetInt.ofArray(Array_data_util.randomRange(0, 100))));

b("File \"bs_set_int_test.ml\", line 140, characters 4-11", Bs_SetInt.eq(a2, a3));

b("File \"bs_set_int_test.ml\", line 141, characters 4-11", match$1[1]);

b("File \"bs_set_int_test.ml\", line 142, characters 4-11", Bs_SetInt.eq(a3, a4));

var a6 = Bs_SetInt.remove(Bs_SetInt.removeMany(a1, Array_data_util.randomRange(0, 39)), 40);

b("File \"bs_set_int_test.ml\", line 144, characters 4-11", Bs_SetInt.eq(a5, a6));

var a7 = Bs_SetInt.remove(a1, 40);

var match$3 = Bs_SetInt.split(a7, 40);

var match$4 = match$3[0];

var a9 = match$4[1];

b("File \"bs_set_int_test.ml\", line 147, characters 4-11", 1 - match$3[1]);

b("File \"bs_set_int_test.ml\", line 148, characters 4-11", Bs_SetInt.eq(a4, match$4[0]));

b("File \"bs_set_int_test.ml\", line 149, characters 4-11", Bs_SetInt.eq(a5, a9));

var a10 = Bs_SetInt.removeMany(a9, Array_data_util.randomRange(42, 2000));

eq("File \"bs_set_int_test.ml\", line 151, characters 5-12", Bs_SetInt.size(a10), 1);

var a11 = Bs_SetInt.removeMany(a9, Array_data_util.randomRange(0, 2000));

b("File \"bs_set_int_test.ml\", line 153, characters 4-11", Bs_SetInt.isEmpty(a11));

Mt.from_pair_suites("bs_set_int_test.ml", suites[0]);

var N = 0;

var I = 0;

var A = 0;

var ofA = Bs_SetInt.ofArray;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.I = I;
exports.A = A;
exports.$eq$tilde = $eq$tilde;
exports.$eq$star = $eq$star;
exports.ofA = ofA;
exports.u = u;
exports.range = range;
exports.revRange = revRange;
/*  Not a pure module */
