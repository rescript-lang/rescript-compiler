'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_SetInt = require("../../lib/js/belt_SetInt.js");
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
  return Belt_SetInt.eq(Belt_SetInt.ofArray(i), s);
}

function $eq$star(a, b) {
  return Belt_SetInt.eq(Belt_SetInt.ofArray(a), Belt_SetInt.ofArray(b));
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

var u = Belt_SetInt.intersect(Belt_SetInt.ofArray(/* int array */[
          1,
          2,
          3
        ]), Belt_SetInt.ofArray(/* int array */[
          3,
          4,
          5
        ]));

b("File \"bs_set_int_test.ml\", line 23, characters 4-11", Belt_SetInt.eq(Belt_SetInt.ofArray(/* int array */[3]), u));

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

var v = Belt_SetInt.ofArray($$Array.append(range(100, 1000), revRange(400, 1500)));

var i = range(100, 1500);

b("File \"bs_set_int_test.ml\", line 36, characters 4-11", Belt_SetInt.eq(Belt_SetInt.ofArray(i), v));

var match = Belt_SetInt.partition(v, (function (x) {
        return +(x % 3 === 0);
      }));

var l = Belt_SetInt.empty;

var r = Belt_SetInt.empty;

for(var i$1 = 100; i$1 <= 1500; ++i$1){
  if (i$1 % 3) {
    r = Belt_SetInt.add(r, i$1);
  } else {
    l = Belt_SetInt.add(l, i$1);
  }
}

b("File \"bs_set_int_test.ml\", line 47, characters 4-11", Belt_SetInt.eq(match[0], l));

b("File \"bs_set_int_test.ml\", line 48, characters 4-11", Belt_SetInt.eq(match[1], r));

var i$2 = range(50, 100);

var s = Belt_SetInt.intersect(Belt_SetInt.ofArray(range(1, 100)), Belt_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 51, characters 4-11", Belt_SetInt.eq(Belt_SetInt.ofArray(i$2), s));

var i$3 = range(1, 200);

var s$1 = Belt_SetInt.union(Belt_SetInt.ofArray(range(1, 100)), Belt_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 54, characters 4-11", Belt_SetInt.eq(Belt_SetInt.ofArray(i$3), s$1));

var i$4 = range(1, 49);

var s$2 = Belt_SetInt.diff(Belt_SetInt.ofArray(range(1, 100)), Belt_SetInt.ofArray(range(50, 200)));

b("File \"bs_set_int_test.ml\", line 57, characters 6-13", Belt_SetInt.eq(Belt_SetInt.ofArray(i$4), s$2));

var i$5 = revRange(50, 100);

var s$3 = Belt_SetInt.intersect(Belt_SetInt.ofArray(revRange(1, 100)), Belt_SetInt.ofArray(revRange(50, 200)));

b("File \"bs_set_int_test.ml\", line 60, characters 4-11", Belt_SetInt.eq(Belt_SetInt.ofArray(i$5), s$3));

var i$6 = revRange(1, 200);

var s$4 = Belt_SetInt.union(Belt_SetInt.ofArray(revRange(1, 100)), Belt_SetInt.ofArray(revRange(50, 200)));

b("File \"bs_set_int_test.ml\", line 63, characters 4-11", Belt_SetInt.eq(Belt_SetInt.ofArray(i$6), s$4));

var i$7 = revRange(1, 49);

var s$5 = Belt_SetInt.diff(Belt_SetInt.ofArray(revRange(1, 100)), Belt_SetInt.ofArray(revRange(50, 200)));

b("File \"bs_set_int_test.ml\", line 66, characters 6-13", Belt_SetInt.eq(Belt_SetInt.ofArray(i$7), s$5));

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

var v$1 = Belt_SetInt.ofArray(/* array */[
      1,
      222,
      3,
      4,
      2,
      0,
      33,
      -1
    ]);

var minv = Belt_SetInt.minUndefined(v$1);

var maxv = Belt_SetInt.maxUndefined(v$1);

function approx(loc, x, y) {
  return b(loc, +(x === y));
}

eq("File \"bs_set_int_test.ml\", line 74, characters 5-12", Belt_SetInt.reduce(v$1, 0, (function (x, y) {
            return x + y | 0;
          })), Belt_Array.reduce(ss, 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })));

approx("File \"bs_set_int_test.ml\", line 75, characters 9-16", -1, minv);

approx("File \"bs_set_int_test.ml\", line 76, characters 9-16", 222, maxv);

var v$2 = Belt_SetInt.remove(v$1, 3);

var minv$1 = Belt_SetInt.minimum(v$2);

var maxv$1 = Belt_SetInt.maximum(v$2);

eq("File \"bs_set_int_test.ml\", line 79, characters 5-12", minv$1, /* Some */[-1]);

eq("File \"bs_set_int_test.ml\", line 80, characters 5-12", maxv$1, /* Some */[222]);

var v$3 = Belt_SetInt.remove(v$2, 222);

var minv$2 = Belt_SetInt.minimum(v$3);

var maxv$2 = Belt_SetInt.maximum(v$3);

eq("File \"bs_set_int_test.ml\", line 83, characters 5-12", minv$2, /* Some */[-1]);

eq("File \"bs_set_int_test.ml\", line 84, characters 5-12", maxv$2, /* Some */[33]);

var v$4 = Belt_SetInt.remove(v$3, -1);

var minv$3 = Belt_SetInt.minimum(v$4);

var maxv$3 = Belt_SetInt.maximum(v$4);

eq("File \"bs_set_int_test.ml\", line 87, characters 5-12", minv$3, /* Some */[0]);

eq("File \"bs_set_int_test.ml\", line 88, characters 5-12", maxv$3, /* Some */[33]);

var v$5 = Belt_SetInt.remove(v$4, 0);

var v$6 = Belt_SetInt.remove(v$5, 33);

var v$7 = Belt_SetInt.remove(v$6, 2);

var v$8 = Belt_SetInt.remove(v$7, 3);

var v$9 = Belt_SetInt.remove(v$8, 4);

var v$10 = Belt_SetInt.remove(v$9, 1);

b("File \"bs_set_int_test.ml\", line 95, characters 4-11", Belt_SetInt.isEmpty(v$10));

var v$11 = Belt_Array.makeByAndShuffle(1000000, (function (i) {
        return i;
      }));

var u$1 = Belt_SetInt.ofArray(v$11);

Belt_SetInt.checkInvariantInternal(u$1);

var firstHalf = Belt_Array.slice(v$11, 0, 2000);

var xx = Belt_Array.reduce(firstHalf, u$1, Belt_SetInt.remove);

Belt_SetInt.checkInvariantInternal(u$1);

b("File \"bs_set_int_test.ml\", line 106, characters 4-11", Belt_SetInt.eq(Belt_SetInt.union(Belt_SetInt.ofArray(firstHalf), xx), u$1));

var aa = Belt_SetInt.ofArray(Array_data_util.randomRange(0, 100));

var bb = Belt_SetInt.ofArray(Array_data_util.randomRange(0, 200));

var cc = Belt_SetInt.ofArray(Array_data_util.randomRange(120, 200));

var dd = Belt_SetInt.union(aa, cc);

b("File \"bs_set_int_test.ml\", line 113, characters 4-11", Belt_SetInt.subset(aa, bb));

b("File \"bs_set_int_test.ml\", line 114, characters 4-11", Belt_SetInt.subset(dd, bb));

b("File \"bs_set_int_test.ml\", line 115, characters 4-11", Belt_SetInt.subset(Belt_SetInt.add(dd, 200), bb));

b("File \"bs_set_int_test.ml\", line 116, characters 4-11", +(Belt_SetInt.add(dd, 200) === dd));

b("File \"bs_set_int_test.ml\", line 117, characters 4-11", +(Belt_SetInt.add(dd, 0) === dd));

b("File \"bs_set_int_test.ml\", line 118, characters 4-11", 1 - Belt_SetInt.subset(Belt_SetInt.add(dd, 201), bb));

var aa$1 = Belt_SetInt.ofArray(Array_data_util.randomRange(0, 100));

var bb$1 = Belt_SetInt.ofArray(Array_data_util.randomRange(0, 100));

var cc$1 = Belt_SetInt.add(bb$1, 101);

var dd$1 = Belt_SetInt.remove(bb$1, 99);

var ee = Belt_SetInt.add(dd$1, 101);

b("File \"bs_set_int_test.ml\", line 127, characters 4-11", Belt_SetInt.eq(aa$1, bb$1));

b("File \"bs_set_int_test.ml\", line 128, characters 4-11", 1 - Belt_SetInt.eq(aa$1, cc$1));

b("File \"bs_set_int_test.ml\", line 129, characters 4-11", 1 - Belt_SetInt.eq(dd$1, cc$1));

b("File \"bs_set_int_test.ml\", line 130, characters 4-11", 1 - Belt_SetInt.eq(bb$1, ee));

var a1 = Belt_SetInt.mergeMany(Belt_SetInt.empty, Array_data_util.randomRange(0, 100));

var a2 = Belt_SetInt.removeMany(a1, Array_data_util.randomRange(40, 100));

var a3 = Belt_SetInt.ofArray(Array_data_util.randomRange(0, 39));

var match$1 = Belt_SetInt.split(a1, 40);

var match$2 = match$1[0];

var a5 = match$2[1];

var a4 = match$2[0];

b("File \"bs_set_int_test.ml\", line 138, characters 4-11", Belt_SetInt.eq(a1, Belt_SetInt.ofArray(Array_data_util.randomRange(0, 100))));

b("File \"bs_set_int_test.ml\", line 139, characters 4-11", Belt_SetInt.eq(a2, a3));

b("File \"bs_set_int_test.ml\", line 140, characters 4-11", match$1[1]);

b("File \"bs_set_int_test.ml\", line 141, characters 4-11", Belt_SetInt.eq(a3, a4));

var a6 = Belt_SetInt.remove(Belt_SetInt.removeMany(a1, Array_data_util.randomRange(0, 39)), 40);

b("File \"bs_set_int_test.ml\", line 143, characters 4-11", Belt_SetInt.eq(a5, a6));

var a7 = Belt_SetInt.remove(a1, 40);

var match$3 = Belt_SetInt.split(a7, 40);

var match$4 = match$3[0];

var a9 = match$4[1];

b("File \"bs_set_int_test.ml\", line 146, characters 4-11", 1 - match$3[1]);

b("File \"bs_set_int_test.ml\", line 147, characters 4-11", Belt_SetInt.eq(a4, match$4[0]));

b("File \"bs_set_int_test.ml\", line 148, characters 4-11", Belt_SetInt.eq(a5, a9));

var a10 = Belt_SetInt.removeMany(a9, Array_data_util.randomRange(42, 2000));

eq("File \"bs_set_int_test.ml\", line 150, characters 5-12", Belt_SetInt.size(a10), 1);

var a11 = Belt_SetInt.removeMany(a9, Array_data_util.randomRange(0, 2000));

b("File \"bs_set_int_test.ml\", line 152, characters 4-11", Belt_SetInt.isEmpty(a11));

var match$5 = Belt_SetInt.split(Belt_SetInt.empty, 0);

var match$6 = match$5[0];

b("File \"bs_set_int_test.ml\", line 156, characters 4-11", Belt_SetInt.isEmpty(match$6[0]));

b("File \"bs_set_int_test.ml\", line 157, characters 4-11", Belt_SetInt.isEmpty(match$6[1]));

b("File \"bs_set_int_test.ml\", line 158, characters 4-11", 1 - match$5[1]);

var v$12 = Belt_SetInt.ofArray(Array_data_util.randomRange(0, 2000));

var v0 = Belt_SetInt.ofArray(Array_data_util.randomRange(0, 2000));

var v1 = Belt_SetInt.ofArray(Array_data_util.randomRange(1, 2001));

var v2 = Belt_SetInt.ofArray(Array_data_util.randomRange(3, 2002));

var v3 = Belt_SetInt.removeMany(v2, /* int array */[
      2002,
      2001
    ]);

var us = Belt_Array.map(Array_data_util.randomRange(1000, 3000), (function (x) {
        return Belt_SetInt.has(v$12, x);
      }));

var counted = Belt_Array.reduce(us, 0, (function (acc, x) {
        if (x) {
          return acc + 1 | 0;
        } else {
          return acc;
        }
      }));

eq("File \"bs_set_int_test.ml\", line 168, characters 5-12", counted, 1001);

b("File \"bs_set_int_test.ml\", line 169, characters 4-11", Belt_SetInt.eq(v$12, v0));

b("File \"bs_set_int_test.ml\", line 170, characters 4-11", +(Belt_SetInt.cmp(v$12, v0) === 0));

b("File \"bs_set_int_test.ml\", line 171, characters 4-11", +(Belt_SetInt.cmp(v$12, v1) < 0));

b("File \"bs_set_int_test.ml\", line 172, characters 4-11", +(Belt_SetInt.cmp(v$12, v2) > 0));

b("File \"bs_set_int_test.ml\", line 173, characters 4-11", Belt_SetInt.subset(v3, v0));

b("File \"bs_set_int_test.ml\", line 174, characters 4-11", 1 - Belt_SetInt.subset(v1, v0));

eq("File \"bs_set_int_test.ml\", line 175, characters 5-12", Belt_SetInt.get(v$12, 30), /* Some */[30]);

eq("File \"bs_set_int_test.ml\", line 176, characters 5-12", Belt_SetInt.get(v$12, 3000), /* None */0);

Mt.from_pair_suites("bs_set_int_test.ml", suites[0]);

var N = 0;

var I = 0;

var A = 0;

var ofA = Belt_SetInt.ofArray;

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
