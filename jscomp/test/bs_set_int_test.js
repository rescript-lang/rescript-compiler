'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Belt_Set = require("../../lib/js/belt_Set.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_SetDict = require("../../lib/js/belt_SetDict.js");
var Array_data_util = require("./array_data_util.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, v) {
  return Mt.bool_suites(test_id, suites, loc, v);
}

function ofA(a) {
  return Belt_Set.fromArray(a, Belt_Set.intId);
}

var empty = {
  cmp: Belt_Set.intId[/* cmp */0],
  data: Belt_SetDict.empty
};

var N = /* module */[
  /* Int */0,
  /* String */0,
  /* Dict */0,
  /* make */Belt_Set.make,
  /* intId */Belt_Set.intId,
  /* fromSetInt */Belt_Set.fromSetInt,
  /* toSetInt */Belt_Set.toSetInt,
  /* ofArray */Belt_Set.ofArray,
  /* ofSortedArrayUnsafe */Belt_Set.ofSortedArrayUnsafe,
  /* fromSortedArrayUnsafe */Belt_Set.fromSortedArrayUnsafe,
  /* isEmpty */Belt_Set.isEmpty,
  /* has */Belt_Set.has,
  /* add */Belt_Set.add,
  /* mergeMany */Belt_Set.mergeMany,
  /* remove */Belt_Set.remove,
  /* removeMany */Belt_Set.removeMany,
  /* union */Belt_Set.union,
  /* intersect */Belt_Set.intersect,
  /* diff */Belt_Set.diff,
  /* subset */Belt_Set.subset,
  /* cmp */Belt_Set.cmp,
  /* eq */Belt_Set.eq,
  /* forEachU */Belt_Set.forEachU,
  /* forEach */Belt_Set.forEach,
  /* reduceU */Belt_Set.reduceU,
  /* reduce */Belt_Set.reduce,
  /* everyU */Belt_Set.everyU,
  /* every */Belt_Set.every,
  /* someU */Belt_Set.someU,
  /* some */Belt_Set.some,
  /* keepU */Belt_Set.keepU,
  /* keep */Belt_Set.keep,
  /* partitionU */Belt_Set.partitionU,
  /* partition */Belt_Set.partition,
  /* size */Belt_Set.size,
  /* toArray */Belt_Set.toArray,
  /* toList */Belt_Set.toList,
  /* minimum */Belt_Set.minimum,
  /* minUndefined */Belt_Set.minUndefined,
  /* maximum */Belt_Set.maximum,
  /* maxUndefined */Belt_Set.maxUndefined,
  /* get */Belt_Set.get,
  /* getUndefined */Belt_Set.getUndefined,
  /* getExn */Belt_Set.getExn,
  /* split */Belt_Set.split,
  /* checkInvariantInternal */Belt_Set.checkInvariantInternal,
  /* getData */Belt_Set.getData,
  /* getId */Belt_Set.getId,
  /* packIdData */Belt_Set.packIdData,
  /* empty */empty,
  /* fromArray */ofA
];

function $eq$tilde(s, i) {
  return Belt_Set.eq(Belt_Set.fromArray(i, Belt_Set.intId), s);
}

function $eq$star(a, b) {
  return Belt_Set.eq(Belt_Set.fromArray(a, Belt_Set.intId), Belt_Set.fromArray(b, Belt_Set.intId));
}

b("File \"bs_set_int_test.ml\", line 22, characters 4-11", $eq$star(/* array */[
          1,
          2,
          3
        ], /* array */[
          3,
          2,
          1
        ]));

var u = Belt_Set.intersect(Belt_Set.fromArray(/* array */[
          1,
          2,
          3
        ], Belt_Set.intId), Belt_Set.fromArray(/* array */[
          3,
          4,
          5
        ], Belt_Set.intId));

b("File \"bs_set_int_test.ml\", line 28, characters 4-11", Belt_Set.eq(Belt_Set.fromArray(/* array */[3], Belt_Set.intId), u));

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

var a = $$Array.append(range(100, 1000), revRange(400, 1500));

var v = Belt_Set.fromArray(a, Belt_Set.intId);

var i = range(100, 1500);

b("File \"bs_set_int_test.ml\", line 41, characters 4-11", Belt_Set.eq(Belt_Set.fromArray(i, Belt_Set.intId), v));

var match = Belt_Set.partition(v, (function (x) {
        return x % 3 === 0;
      }));

var l = empty;

var r = empty;

for(var i$1 = 100; i$1 <= 1500; ++i$1){
  if (i$1 % 3 === 0) {
    l = Belt_Set.add(l, i$1);
  } else {
    r = Belt_Set.add(r, i$1);
  }
}

var nl = l;

var nr = r;

b("File \"bs_set_int_test.ml\", line 52, characters 4-11", Belt_Set.eq(match[0], nl));

b("File \"bs_set_int_test.ml\", line 53, characters 4-11", Belt_Set.eq(match[1], nr));

var i$2 = range(50, 100);

var a$1 = range(1, 100);

var a$2 = range(50, 200);

var s = Belt_Set.intersect(Belt_Set.fromArray(a$1, Belt_Set.intId), Belt_Set.fromArray(a$2, Belt_Set.intId));

b("File \"bs_set_int_test.ml\", line 56, characters 4-11", Belt_Set.eq(Belt_Set.fromArray(i$2, Belt_Set.intId), s));

var i$3 = range(1, 200);

var a$3 = range(1, 100);

var a$4 = range(50, 200);

var s$1 = Belt_Set.union(Belt_Set.fromArray(a$3, Belt_Set.intId), Belt_Set.fromArray(a$4, Belt_Set.intId));

b("File \"bs_set_int_test.ml\", line 59, characters 4-11", Belt_Set.eq(Belt_Set.fromArray(i$3, Belt_Set.intId), s$1));

var i$4 = range(1, 49);

var a$5 = range(1, 100);

var a$6 = range(50, 200);

var s$2 = Belt_Set.diff(Belt_Set.fromArray(a$5, Belt_Set.intId), Belt_Set.fromArray(a$6, Belt_Set.intId));

b("File \"bs_set_int_test.ml\", line 62, characters 6-13", Belt_Set.eq(Belt_Set.fromArray(i$4, Belt_Set.intId), s$2));

var i$5 = revRange(50, 100);

var a$7 = revRange(1, 100);

var a$8 = revRange(50, 200);

var s$3 = Belt_Set.intersect(Belt_Set.fromArray(a$7, Belt_Set.intId), Belt_Set.fromArray(a$8, Belt_Set.intId));

b("File \"bs_set_int_test.ml\", line 65, characters 4-11", Belt_Set.eq(Belt_Set.fromArray(i$5, Belt_Set.intId), s$3));

var i$6 = revRange(1, 200);

var a$9 = revRange(1, 100);

var a$10 = revRange(50, 200);

var s$4 = Belt_Set.union(Belt_Set.fromArray(a$9, Belt_Set.intId), Belt_Set.fromArray(a$10, Belt_Set.intId));

b("File \"bs_set_int_test.ml\", line 68, characters 4-11", Belt_Set.eq(Belt_Set.fromArray(i$6, Belt_Set.intId), s$4));

var i$7 = revRange(1, 49);

var a$11 = revRange(1, 100);

var a$12 = revRange(50, 200);

var s$5 = Belt_Set.diff(Belt_Set.fromArray(a$11, Belt_Set.intId), Belt_Set.fromArray(a$12, Belt_Set.intId));

b("File \"bs_set_int_test.ml\", line 71, characters 6-13", Belt_Set.eq(Belt_Set.fromArray(i$7, Belt_Set.intId), s$5));

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

var v$1 = Belt_Set.fromArray(/* array */[
      1,
      222,
      3,
      4,
      2,
      0,
      33,
      -1
    ], Belt_Set.intId);

var minv = Belt_SetDict.minUndefined(v$1.data);

var maxv = Belt_SetDict.maxUndefined(v$1.data);

function approx(loc, x, y) {
  return b(loc, x === y);
}

eq("File \"bs_set_int_test.ml\", line 79, characters 5-12", Belt_Set.reduce(v$1, 0, (function (x, y) {
            return x + y | 0;
          })), Belt_Array.reduce(ss, 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })));

approx("File \"bs_set_int_test.ml\", line 80, characters 9-16", -1, minv);

approx("File \"bs_set_int_test.ml\", line 81, characters 9-16", 222, maxv);

var v$2 = Belt_Set.remove(v$1, 3);

var minv$1 = Belt_SetDict.minimum(v$2.data);

var maxv$1 = Belt_SetDict.maximum(v$2.data);

eq("File \"bs_set_int_test.ml\", line 84, characters 5-12", minv$1, /* Some */[-1]);

eq("File \"bs_set_int_test.ml\", line 85, characters 5-12", maxv$1, /* Some */[222]);

var v$3 = Belt_Set.remove(v$2, 222);

var minv$2 = Belt_SetDict.minimum(v$3.data);

var maxv$2 = Belt_SetDict.maximum(v$3.data);

eq("File \"bs_set_int_test.ml\", line 88, characters 5-12", minv$2, /* Some */[-1]);

eq("File \"bs_set_int_test.ml\", line 89, characters 5-12", maxv$2, /* Some */[33]);

var v$4 = Belt_Set.remove(v$3, -1);

var minv$3 = Belt_SetDict.minimum(v$4.data);

var maxv$3 = Belt_SetDict.maximum(v$4.data);

eq("File \"bs_set_int_test.ml\", line 92, characters 5-12", minv$3, /* Some */[0]);

eq("File \"bs_set_int_test.ml\", line 93, characters 5-12", maxv$3, /* Some */[33]);

var v$5 = Belt_Set.remove(v$4, 0);

var v$6 = Belt_Set.remove(v$5, 33);

var v$7 = Belt_Set.remove(v$6, 2);

var v$8 = Belt_Set.remove(v$7, 3);

var v$9 = Belt_Set.remove(v$8, 4);

var v$10 = Belt_Set.remove(v$9, 1);

b("File \"bs_set_int_test.ml\", line 100, characters 4-11", Belt_SetDict.isEmpty(v$10.data));

var v$11 = Belt_Array.makeByAndShuffle(1000000, (function (i) {
        return i;
      }));

var u$1 = Belt_Set.fromArray(v$11, Belt_Set.intId);

Belt_SetDict.checkInvariantInternal(u$1.data);

var firstHalf = Belt_Array.slice(v$11, 0, 2000);

var xx = Belt_Array.reduce(firstHalf, u$1, Belt_Set.remove);

Belt_SetDict.checkInvariantInternal(u$1.data);

b("File \"bs_set_int_test.ml\", line 111, characters 4-11", Belt_Set.eq(Belt_Set.union(Belt_Set.fromArray(firstHalf, Belt_Set.intId), xx), u$1));

var a$13 = Array_data_util.randomRange(0, 100);

var aa = Belt_Set.fromArray(a$13, Belt_Set.intId);

var a$14 = Array_data_util.randomRange(0, 200);

var bb = Belt_Set.fromArray(a$14, Belt_Set.intId);

var a$15 = Array_data_util.randomRange(120, 200);

var cc = Belt_Set.fromArray(a$15, Belt_Set.intId);

var dd = Belt_Set.union(aa, cc);

b("File \"bs_set_int_test.ml\", line 118, characters 4-11", Belt_Set.subset(aa, bb));

b("File \"bs_set_int_test.ml\", line 119, characters 4-11", Belt_Set.subset(dd, bb));

b("File \"bs_set_int_test.ml\", line 120, characters 4-11", Belt_Set.subset(Belt_Set.add(dd, 200), bb));

b("File \"bs_set_int_test.ml\", line 121, characters 4-11", Belt_Set.add(dd, 200) === dd);

b("File \"bs_set_int_test.ml\", line 122, characters 4-11", Belt_Set.add(dd, 0) === dd);

b("File \"bs_set_int_test.ml\", line 123, characters 4-11", !Belt_Set.subset(Belt_Set.add(dd, 201), bb));

var a$16 = Array_data_util.randomRange(0, 100);

var aa$1 = Belt_Set.fromArray(a$16, Belt_Set.intId);

var a$17 = Array_data_util.randomRange(0, 100);

var bb$1 = Belt_Set.fromArray(a$17, Belt_Set.intId);

var cc$1 = Belt_Set.add(bb$1, 101);

var dd$1 = Belt_Set.remove(bb$1, 99);

var ee = Belt_Set.add(dd$1, 101);

b("File \"bs_set_int_test.ml\", line 132, characters 4-11", Belt_Set.eq(aa$1, bb$1));

b("File \"bs_set_int_test.ml\", line 133, characters 4-11", !Belt_Set.eq(aa$1, cc$1));

b("File \"bs_set_int_test.ml\", line 134, characters 4-11", !Belt_Set.eq(dd$1, cc$1));

b("File \"bs_set_int_test.ml\", line 135, characters 4-11", !Belt_Set.eq(bb$1, ee));

var a1 = Belt_Set.mergeMany(empty, Array_data_util.randomRange(0, 100));

var a2 = Belt_Set.removeMany(a1, Array_data_util.randomRange(40, 100));

var a$18 = Array_data_util.randomRange(0, 39);

var a3 = Belt_Set.fromArray(a$18, Belt_Set.intId);

var match$1 = Belt_Set.split(a1, 40);

var match$2 = match$1[0];

var a5 = match$2[1];

var a4 = match$2[0];

var a$19 = Array_data_util.randomRange(0, 100);

b("File \"bs_set_int_test.ml\", line 143, characters 4-11", Belt_Set.eq(a1, Belt_Set.fromArray(a$19, Belt_Set.intId)));

b("File \"bs_set_int_test.ml\", line 144, characters 4-11", Belt_Set.eq(a2, a3));

b("File \"bs_set_int_test.ml\", line 145, characters 4-11", match$1[1]);

b("File \"bs_set_int_test.ml\", line 146, characters 4-11", Belt_Set.eq(a3, a4));

var a6 = Belt_Set.remove(Belt_Set.removeMany(a1, Array_data_util.randomRange(0, 39)), 40);

b("File \"bs_set_int_test.ml\", line 148, characters 4-11", Belt_Set.eq(a5, a6));

var a7 = Belt_Set.remove(a1, 40);

var match$3 = Belt_Set.split(a7, 40);

var match$4 = match$3[0];

var a9 = match$4[1];

b("File \"bs_set_int_test.ml\", line 151, characters 4-11", !match$3[1]);

b("File \"bs_set_int_test.ml\", line 152, characters 4-11", Belt_Set.eq(a4, match$4[0]));

b("File \"bs_set_int_test.ml\", line 153, characters 4-11", Belt_Set.eq(a5, a9));

var a10 = Belt_Set.removeMany(a9, Array_data_util.randomRange(42, 2000));

eq("File \"bs_set_int_test.ml\", line 155, characters 5-12", Belt_SetDict.size(a10.data), 1);

var a11 = Belt_Set.removeMany(a9, Array_data_util.randomRange(0, 2000));

b("File \"bs_set_int_test.ml\", line 157, characters 4-11", Belt_SetDict.isEmpty(a11.data));

var match$5 = Belt_Set.split(empty, 0);

var match$6 = match$5[0];

b("File \"bs_set_int_test.ml\", line 161, characters 4-11", Belt_SetDict.isEmpty(match$6[0].data));

b("File \"bs_set_int_test.ml\", line 162, characters 4-11", Belt_SetDict.isEmpty(match$6[1].data));

b("File \"bs_set_int_test.ml\", line 163, characters 4-11", !match$5[1]);

var a$20 = Array_data_util.randomRange(0, 2000);

var v$12 = Belt_Set.fromArray(a$20, Belt_Set.intId);

var a$21 = Array_data_util.randomRange(0, 2000);

var v0 = Belt_Set.fromArray(a$21, Belt_Set.intId);

var a$22 = Array_data_util.randomRange(1, 2001);

var v1 = Belt_Set.fromArray(a$22, Belt_Set.intId);

var a$23 = Array_data_util.randomRange(3, 2002);

var v2 = Belt_Set.fromArray(a$23, Belt_Set.intId);

var v3 = Belt_Set.removeMany(v2, /* array */[
      2002,
      2001
    ]);

var us = Belt_Array.map(Array_data_util.randomRange(1000, 3000), (function (x) {
        return Belt_Set.has(v$12, x);
      }));

var counted = Belt_Array.reduce(us, 0, (function (acc, x) {
        if (x) {
          return acc + 1 | 0;
        } else {
          return acc;
        }
      }));

eq("File \"bs_set_int_test.ml\", line 173, characters 5-12", counted, 1001);

b("File \"bs_set_int_test.ml\", line 174, characters 4-11", Belt_Set.eq(v$12, v0));

b("File \"bs_set_int_test.ml\", line 175, characters 4-11", Belt_Set.cmp(v$12, v0) === 0);

b("File \"bs_set_int_test.ml\", line 176, characters 4-11", Belt_Set.cmp(v$12, v1) < 0);

b("File \"bs_set_int_test.ml\", line 177, characters 4-11", Belt_Set.cmp(v$12, v2) > 0);

b("File \"bs_set_int_test.ml\", line 178, characters 4-11", Belt_Set.subset(v3, v0));

b("File \"bs_set_int_test.ml\", line 179, characters 4-11", !Belt_Set.subset(v1, v0));

eq("File \"bs_set_int_test.ml\", line 180, characters 5-12", Belt_Set.get(v$12, 30), /* Some */[30]);

eq("File \"bs_set_int_test.ml\", line 181, characters 5-12", Belt_Set.get(v$12, 3000), /* None */0);

Mt.from_pair_suites("bs_set_int_test.ml", suites[0]);

var I = 0;

var A = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.ofA = ofA;
exports.N = N;
exports.I = I;
exports.A = A;
exports.$eq$tilde = $eq$tilde;
exports.$eq$star = $eq$star;
exports.u = u;
exports.range = range;
exports.revRange = revRange;
/*  Not a pure module */
