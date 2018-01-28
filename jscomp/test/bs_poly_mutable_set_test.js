'use strict';

var Mt = require("./mt.js");
var Bs_List = require("../../lib/js/bs_List.js");
var Bs_SetM = require("../../lib/js/bs_SetM.js");
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

eq("File \"bs_poly_mutable_set_test.ml\", line 19, characters 5-12", Bs_internalAVLset.length0(u.data), 28);

var r = Array_data_util.randomRange(0, 30);

b("File \"bs_poly_mutable_set_test.ml\", line 21, characters 4-11", +(29 === Bs_internalAVLset.maxUndefined0(u.data)));

b("File \"bs_poly_mutable_set_test.ml\", line 22, characters 4-11", +(1 === Bs_internalAVLset.minUndefined0(u.data)));

Bs_SetM.add(u, 3);

for(var i = 0 ,i_finish = r.length - 1 | 0; i <= i_finish; ++i){
  Bs_SetM.remove(u, r[i]);
}

b("File \"bs_poly_mutable_set_test.ml\", line 27, characters 4-11", Bs_internalAVLset.isEmpty0(u.data));

Bs_SetM.add(u, 0);

Bs_SetM.add(u, 1);

Bs_SetM.add(u, 2);

Bs_SetM.add(u, 0);

eq("File \"bs_poly_mutable_set_test.ml\", line 32, characters 5-12", Bs_internalAVLset.length0(u.data), 3);

b("File \"bs_poly_mutable_set_test.ml\", line 33, characters 4-11", 1 - Bs_internalAVLset.isEmpty0(u.data));

for(var i$1 = 0; i$1 <= 3; ++i$1){
  Bs_SetM.remove(u, i$1);
}

b("File \"bs_poly_mutable_set_test.ml\", line 37, characters 4-11", Bs_internalAVLset.isEmpty0(u.data));

Bs_SetM.mergeMany(u, Array_data_util.randomRange(0, 20000));

Bs_SetM.mergeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_poly_mutable_set_test.ml\", line 40, characters 5-12", Bs_internalAVLset.length0(u.data), 20001);

Bs_SetM.removeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_poly_mutable_set_test.ml\", line 42, characters 5-12", Bs_internalAVLset.length0(u.data), 19800);

Bs_SetM.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_poly_mutable_set_test.ml\", line 44, characters 5-12", Bs_internalAVLset.length0(u.data), 19000);

Bs_SetM.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_poly_mutable_set_test.ml\", line 46, characters 5-12", Bs_internalAVLset.length0(u.data), 19000);

Bs_SetM.removeMany(u, Array_data_util.randomRange(1000, 10000));

eq("File \"bs_poly_mutable_set_test.ml\", line 48, characters 5-12", Bs_internalAVLset.length0(u.data), 10000);

Bs_SetM.removeMany(u, Array_data_util.randomRange(10000, 19999));

eq("File \"bs_poly_mutable_set_test.ml\", line 50, characters 5-12", Bs_internalAVLset.length0(u.data), 1);

b("File \"bs_poly_mutable_set_test.ml\", line 51, characters 4-11", Bs_SetM.has(u, 20000));

function f(param) {
  return Bs_SetM.ofArray(param, IntCmp);
}

var aa = f(Array_data_util.randomRange(0, 100));

var bb = f(Array_data_util.randomRange(40, 120));

var cc = Bs_SetM.union(aa, bb);

b("File \"bs_poly_mutable_set_test.ml\", line 60, characters 4-11", Bs_SetM.eq(cc, f(Array_data_util.randomRange(0, 120))));

b("File \"bs_poly_mutable_set_test.ml\", line 62, characters 4-11", Bs_SetM.eq(Bs_SetM.union(f(Array_data_util.randomRange(0, 20)), f(Array_data_util.randomRange(21, 40))), f(Array_data_util.randomRange(0, 40))));

var dd = Bs_SetM.intersect(aa, bb);

b("File \"bs_poly_mutable_set_test.ml\", line 67, characters 4-11", Bs_SetM.eq(dd, f(Array_data_util.randomRange(40, 100))));

b("File \"bs_poly_mutable_set_test.ml\", line 68, characters 4-11", Bs_SetM.eq(Bs_SetM.intersect(f(Array_data_util.randomRange(0, 20)), f(Array_data_util.randomRange(21, 40))), {
          dict: IntCmp,
          data: Bs_internalAVLset.empty0
        }));

b("File \"bs_poly_mutable_set_test.ml\", line 74, characters 4-11", Bs_SetM.eq(Bs_SetM.intersect(f(Array_data_util.randomRange(21, 40)), f(Array_data_util.randomRange(0, 20))), {
          dict: IntCmp,
          data: Bs_internalAVLset.empty0
        }));

b("File \"bs_poly_mutable_set_test.ml\", line 80, characters 4-11", Bs_SetM.eq(Bs_SetM.intersect(f(/* array */[
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

b("File \"bs_poly_mutable_set_test.ml\", line 86, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(aa, bb), f(Array_data_util.randomRange(0, 39))));

b("File \"bs_poly_mutable_set_test.ml\", line 88, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(bb, aa), f(Array_data_util.randomRange(101, 120))));

b("File \"bs_poly_mutable_set_test.ml\", line 90, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(f(Array_data_util.randomRange(21, 40)), f(Array_data_util.randomRange(0, 20))), f(Array_data_util.randomRange(21, 40))));

b("File \"bs_poly_mutable_set_test.ml\", line 96, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(f(Array_data_util.randomRange(0, 20)), f(Array_data_util.randomRange(21, 40))), f(Array_data_util.randomRange(0, 20))));

b("File \"bs_poly_mutable_set_test.ml\", line 103, characters 4-11", Bs_SetM.eq(Bs_SetM.diff(f(Array_data_util.randomRange(0, 20)), f(Array_data_util.randomRange(0, 40))), f(Array_data_util.randomRange(0, -1))));

var a0 = Bs_SetM.ofArray(Array_data_util.randomRange(0, 1000), IntCmp);

var a1 = Bs_SetM.keepBy(a0, (function (x) {
        return +(x % 2 === 0);
      }));

var a2 = Bs_SetM.keepBy(a0, (function (x) {
        return +(x % 2 !== 0);
      }));

var match = Bs_SetM.partition(a0, (function (x) {
        return +(x % 2 === 0);
      }));

var a4 = match[1];

var a3 = match[0];

b("File \"bs_poly_mutable_set_test.ml\", line 118, characters 4-11", Bs_SetM.eq(a1, a3));

b("File \"bs_poly_mutable_set_test.ml\", line 119, characters 4-11", Bs_SetM.eq(a2, a4));

b("File \"bs_poly_mutable_set_test.ml\", line 120, characters 4-11", Bs_List.every(/* :: */[
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
