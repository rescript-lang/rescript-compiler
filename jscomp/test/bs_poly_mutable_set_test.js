'use strict';

var Mt = require("./mt.js");
var Bs_List = require("../../lib/js/bs_List.js");
var Bs_SetM = require("../../lib/js/bs_SetM.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");
var Bs_internalAVLset = require("../../lib/js/bs_internalAVLset.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

var IntCmp = /* module */[/* cmp */Caml_primitive.caml_int_compare];

var u = Bs_SetM.ofArray(Array_data_util.range(0, 30), IntCmp);

b("File \"bs_poly_mutable_set_test.ml\", line 15, characters 4-11", Bs_SetM.removeCheck(u, 0));

b("File \"bs_poly_mutable_set_test.ml\", line 16, characters 4-11", 1 - Bs_SetM.removeCheck(u, 0));

b("File \"bs_poly_mutable_set_test.ml\", line 17, characters 4-11", Bs_SetM.removeCheck(u, 30));

b("File \"bs_poly_mutable_set_test.ml\", line 18, characters 4-11", Bs_SetM.removeCheck(u, 20));

eq("File \"bs_poly_mutable_set_test.ml\", line 19, characters 5-12", Bs_internalAVLset.size(u.data), 28);

var r = Array_data_util.randomRange(0, 30);

b("File \"bs_poly_mutable_set_test.ml\", line 21, characters 4-11", +(29 === Bs_internalAVLset.maxUndefined(u.data)));

b("File \"bs_poly_mutable_set_test.ml\", line 22, characters 4-11", +(1 === Bs_internalAVLset.minUndefined(u.data)));

Bs_SetM.add(u, 3);

for(var i = 0 ,i_finish = r.length - 1 | 0; i <= i_finish; ++i){
  Bs_SetM.remove(u, r[i]);
}

b("File \"bs_poly_mutable_set_test.ml\", line 27, characters 4-11", Bs_internalAVLset.isEmpty(u.data));

Bs_SetM.add(u, 0);

Bs_SetM.add(u, 1);

Bs_SetM.add(u, 2);

Bs_SetM.add(u, 0);

eq("File \"bs_poly_mutable_set_test.ml\", line 32, characters 5-12", Bs_internalAVLset.size(u.data), 3);

b("File \"bs_poly_mutable_set_test.ml\", line 33, characters 4-11", 1 - Bs_internalAVLset.isEmpty(u.data));

for(var i$1 = 0; i$1 <= 3; ++i$1){
  Bs_SetM.remove(u, i$1);
}

b("File \"bs_poly_mutable_set_test.ml\", line 37, characters 4-11", Bs_internalAVLset.isEmpty(u.data));

Bs_SetM.mergeMany(u, Array_data_util.randomRange(0, 20000));

Bs_SetM.mergeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_poly_mutable_set_test.ml\", line 40, characters 5-12", Bs_internalAVLset.size(u.data), 20001);

Bs_SetM.removeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_poly_mutable_set_test.ml\", line 42, characters 5-12", Bs_internalAVLset.size(u.data), 19800);

Bs_SetM.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_poly_mutable_set_test.ml\", line 44, characters 5-12", Bs_internalAVLset.size(u.data), 19000);

Bs_SetM.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_poly_mutable_set_test.ml\", line 46, characters 5-12", Bs_internalAVLset.size(u.data), 19000);

Bs_SetM.removeMany(u, Array_data_util.randomRange(1000, 10000));

eq("File \"bs_poly_mutable_set_test.ml\", line 48, characters 5-12", Bs_internalAVLset.size(u.data), 10000);

Bs_SetM.removeMany(u, Array_data_util.randomRange(10000, 19999));

eq("File \"bs_poly_mutable_set_test.ml\", line 50, characters 5-12", Bs_internalAVLset.size(u.data), 1);

b("File \"bs_poly_mutable_set_test.ml\", line 51, characters 4-11", Bs_SetM.has(u, 20000));

Bs_SetM.removeMany(u, Array_data_util.randomRange(10000, 30000));

b("File \"bs_poly_mutable_set_test.ml\", line 53, characters 4-11", Bs_internalAVLset.isEmpty(u.data));

var v = Bs_SetM.ofArray(Array_data_util.randomRange(1000, 2000), IntCmp);

var bs = Bs_Array.map(Array_data_util.randomRange(500, 1499), (function (x) {
        return Bs_SetM.removeCheck(v, x);
      }));

var indeedRemoved = Bs_Array.reduce(bs, 0, (function (acc, x) {
        if (x) {
          return acc + 1 | 0;
        } else {
          return acc;
        }
      }));

eq("File \"bs_poly_mutable_set_test.ml\", line 60, characters 5-12", indeedRemoved, 500);

eq("File \"bs_poly_mutable_set_test.ml\", line 61, characters 5-12", Bs_internalAVLset.size(v.data), 501);

var cs = Bs_Array.map(Array_data_util.randomRange(500, 2000), (function (x) {
        return Bs_SetM.addCheck(v, x);
      }));

var indeedAded = Bs_Array.reduce(cs, 0, (function (acc, x) {
        if (x) {
          return acc + 1 | 0;
        } else {
          return acc;
        }
      }));

eq("File \"bs_poly_mutable_set_test.ml\", line 64, characters 5-12", indeedAded, 1000);

eq("File \"bs_poly_mutable_set_test.ml\", line 65, characters 5-12", Bs_internalAVLset.size(v.data), 1501);

var d = {
  cmp: IntCmp[/* cmp */0],
  data: Bs_internalAVLset.empty
};

b("File \"bs_poly_mutable_set_test.ml\", line 66, characters 4-11", Bs_internalAVLset.isEmpty(d.data));

eq("File \"bs_poly_mutable_set_test.ml\", line 67, characters 5-12", Bs_internalAVLset.minimum(v.data), /* Some */[500]);

eq("File \"bs_poly_mutable_set_test.ml\", line 68, characters 5-12", Bs_internalAVLset.maximum(v.data), /* Some */[2000]);

eq("File \"bs_poly_mutable_set_test.ml\", line 69, characters 5-12", Bs_internalAVLset.minUndefined(v.data), 500);

eq("File \"bs_poly_mutable_set_test.ml\", line 70, characters 5-12", Bs_internalAVLset.maxUndefined(v.data), 2000);

eq("File \"bs_poly_mutable_set_test.ml\", line 71, characters 5-12", Bs_SetM.reduce(v, 0, (function (x, y) {
            return x + y | 0;
          })), 1876250);

b("File \"bs_poly_mutable_set_test.ml\", line 72, characters 4-11", Bs_List.eq(Bs_internalAVLset.toList(v.data), Bs_List.makeBy(1501, (function (i) {
                return i + 500 | 0;
              })), (function (x, y) {
            return +(x === y);
          })));

eq("File \"bs_poly_mutable_set_test.ml\", line 73, characters 5-12", Bs_internalAVLset.toArray(v.data), Array_data_util.range(500, 2000));

b("File \"bs_poly_mutable_set_test.ml\", line 74, characters 4-11", Bs_internalAVLset.checkInvariantInternal(v.data));

eq("File \"bs_poly_mutable_set_test.ml\", line 75, characters 5-12", Bs_SetM.get(v, 3), /* None */0);

eq("File \"bs_poly_mutable_set_test.ml\", line 76, characters 5-12", Bs_SetM.get(v, 1200), /* Some */[1200]);

var match = Bs_SetM.split(v, 1000);

var match$1 = match[0];

var bb = match$1[1];

var aa = match$1[0];

b("File \"bs_poly_mutable_set_test.ml\", line 78, characters 4-11", match[1]);

b("File \"bs_poly_mutable_set_test.ml\", line 79, characters 4-11", Bs_Array.eq(Bs_internalAVLset.toArray(aa.data), Array_data_util.range(500, 999), (function (x, y) {
            return +(x === y);
          })));

b("File \"bs_poly_mutable_set_test.ml\", line 80, characters 4-11", Bs_Array.eq(Bs_internalAVLset.toArray(bb.data), Array_data_util.range(1001, 2000), (function (x, y) {
            return +(x === y);
          })));

b("File \"bs_poly_mutable_set_test.ml\", line 81, characters 5-12", Bs_SetM.subset(aa, v));

b("File \"bs_poly_mutable_set_test.ml\", line 82, characters 4-11", Bs_SetM.subset(bb, v));

var d$1 = Bs_SetM.intersect(aa, bb);

b("File \"bs_poly_mutable_set_test.ml\", line 83, characters 4-11", Bs_internalAVLset.isEmpty(d$1.data));

var c = Bs_SetM.removeCheck(v, 1000);

b("File \"bs_poly_mutable_set_test.ml\", line 85, characters 4-11", c);

var match$2 = Bs_SetM.split(v, 1000);

var match$3 = match$2[0];

var bb$1 = match$3[1];

var aa$1 = match$3[0];

b("File \"bs_poly_mutable_set_test.ml\", line 87, characters 4-11", 1 - match$2[1]);

b("File \"bs_poly_mutable_set_test.ml\", line 88, characters 4-11", Bs_Array.eq(Bs_internalAVLset.toArray(aa$1.data), Array_data_util.range(500, 999), (function (x, y) {
            return +(x === y);
          })));

b("File \"bs_poly_mutable_set_test.ml\", line 89, characters 4-11", Bs_Array.eq(Bs_internalAVLset.toArray(bb$1.data), Array_data_util.range(1001, 2000), (function (x, y) {
            return +(x === y);
          })));

b("File \"bs_poly_mutable_set_test.ml\", line 90, characters 5-12", Bs_SetM.subset(aa$1, v));

b("File \"bs_poly_mutable_set_test.ml\", line 91, characters 4-11", Bs_SetM.subset(bb$1, v));

var d$2 = Bs_SetM.intersect(aa$1, bb$1);

b("File \"bs_poly_mutable_set_test.ml\", line 92, characters 4-11", Bs_internalAVLset.isEmpty(d$2.data));

function f(param) {
  return Bs_SetM.ofArray(param, IntCmp);
}

var aa$2 = f(Array_data_util.randomRange(0, 100));

var bb$2 = f(Array_data_util.randomRange(40, 120));

var cc = Bs_SetM.union(aa$2, bb$2);

b("File \"bs_poly_mutable_set_test.ml\", line 101, characters 4-11", Bs_SetM.eq(cc, f(Array_data_util.randomRange(0, 120))));

b("File \"bs_poly_mutable_set_test.ml\", line 103, characters 4-11", Bs_SetM.eq(Bs_SetM.union(f(Array_data_util.randomRange(0, 20)), f(Array_data_util.randomRange(21, 40))), f(Array_data_util.randomRange(0, 40))));

var dd = Bs_SetM.intersect(aa$2, bb$2);

b("File \"bs_poly_mutable_set_test.ml\", line 108, characters 4-11", Bs_SetM.eq(dd, f(Array_data_util.randomRange(40, 100))));

b("File \"bs_poly_mutable_set_test.ml\", line 109, characters 4-11", Bs_SetM.eq(Bs_SetM.intersect(f(Array_data_util.randomRange(0, 20)), f(Array_data_util.randomRange(21, 40))), {
          cmp: IntCmp[/* cmp */0],
          data: Bs_internalAVLset.empty
        }));

b("File \"bs_poly_mutable_set_test.ml\", line 115, characters 4-11", Bs_SetM.eq(Bs_SetM.intersect(f(Array_data_util.randomRange(21, 40)), f(Array_data_util.randomRange(0, 20))), {
          cmp: IntCmp[/* cmp */0],
          data: Bs_internalAVLset.empty
        }));

b("File \"bs_poly_mutable_set_test.ml\", line 121, characters 4-11", Bs_SetM.eq(Bs_SetM.intersect(f(/* array */[
                  1,
                  3,
                  4,
                  5,
                  7,
                  9
                ]), f(/* array */[
                  2,
                  4,
                  5,
                  6,
                  8,
                  10
                ])), f(/* int array */[
              4,
              5
            ])));

b("File \"bs_poly_mutable_set_test.ml\", line 127, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(aa$2, bb$2), f(Array_data_util.randomRange(0, 39))));

b("File \"bs_poly_mutable_set_test.ml\", line 129, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(bb$2, aa$2), f(Array_data_util.randomRange(101, 120))));

b("File \"bs_poly_mutable_set_test.ml\", line 131, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(f(Array_data_util.randomRange(21, 40)), f(Array_data_util.randomRange(0, 20))), f(Array_data_util.randomRange(21, 40))));

b("File \"bs_poly_mutable_set_test.ml\", line 137, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(f(Array_data_util.randomRange(0, 20)), f(Array_data_util.randomRange(21, 40))), f(Array_data_util.randomRange(0, 20))));

b("File \"bs_poly_mutable_set_test.ml\", line 144, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(f(Array_data_util.randomRange(0, 20)), f(Array_data_util.randomRange(0, 40))), f(Array_data_util.randomRange(0, -1))));

var a0 = Bs_SetM.ofArray(Array_data_util.randomRange(0, 1000), IntCmp);

var a1 = Bs_SetM.keepBy(a0, (function (x) {
        return +(x % 2 === 0);
      }));

var a2 = Bs_SetM.keepBy(a0, (function (x) {
        return +(x % 2 !== 0);
      }));

var match$4 = Bs_SetM.partition(a0, (function (x) {
        return +(x % 2 === 0);
      }));

var a4 = match$4[1];

var a3 = match$4[0];

b("File \"bs_poly_mutable_set_test.ml\", line 159, characters 4-11", Bs_SetM.eq(a1, a3));

b("File \"bs_poly_mutable_set_test.ml\", line 160, characters 4-11", Bs_SetM.eq(a2, a4));

b("File \"bs_poly_mutable_set_test.ml\", line 161, characters 4-11", Bs_List.every(/* :: */[
          a0,
          /* :: */[
            a1,
            /* :: */[
              a2,
              /* :: */[
                a3,
                /* :: */[
                  a4,
                  /* [] */0
                ]
              ]
            ]
          ]
        ], (function (x) {
            return Bs_internalAVLset.checkInvariantInternal(x.data);
          })));

Mt.from_pair_suites("bs_poly_mutable_set_test.ml", suites[0]);

var N = 0;

var I = 0;

var A = 0;

var L = 0;

var $plus$plus = Bs_SetM.union;

var $eq$tilde = Bs_SetM.eq;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.I = I;
exports.A = A;
exports.IntCmp = IntCmp;
exports.L = L;
exports.$plus$plus = $plus$plus;
exports.f = f;
exports.$eq$tilde = $eq$tilde;
/* u Not a pure module */
