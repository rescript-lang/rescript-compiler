'use strict';

var Mt = require("./mt.js");
var Bs_Set = require("../../lib/js/bs_Set.js");
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

var u0 = Bs_Set.ofArray(IntCmp, Array_data_util.range(0, 30));

var u1 = Bs_Set.remove(u0, 0);

var u2 = Bs_Set.remove(u1, 0);

var u3 = Bs_Set.remove(u2, 30);

var u4 = Bs_Set.remove(u3, 20);

var r = Array_data_util.randomRange(0, 30);

var u5 = Bs_Set.add(u4, 3);

var u6 = Bs_Set.removeArray(u5, r);

var u7 = Bs_Set.addArray(u6, /* int array */[
      0,
      1,
      2,
      0
    ]);

var u8 = Bs_Set.removeArray(u7, /* int array */[
      0,
      1,
      2,
      3
    ]);

var u9 = Bs_Set.addArray(u8, Array_data_util.randomRange(0, 20000));

var u10 = Bs_Set.addArray(u9, Array_data_util.randomRange(0, 200));

var u11 = Bs_Set.removeArray(u10, Array_data_util.randomRange(0, 200));

var u12 = Bs_Set.removeArray(u11, Array_data_util.randomRange(0, 1000));

var u13 = Bs_Set.removeArray(u12, Array_data_util.randomRange(0, 1000));

var u14 = Bs_Set.removeArray(u13, Array_data_util.randomRange(1000, 10000));

var u15 = Bs_Set.removeArray(u14, Array_data_util.randomRange(10000, 19999));

var u16 = Bs_Set.removeArray(u15, Array_data_util.randomRange(20000, 21000));

b("File \"bs_poly_set_test.ml\", line 33, characters 4-11", +(u0 !== u1));

b("File \"bs_poly_set_test.ml\", line 34, characters 4-11", +(u2 === u1));

eq("File \"bs_poly_set_test.ml\", line 35, characters 5-12", Bs_internalAVLset.length0(u4.data), 28);

b("File \"bs_poly_set_test.ml\", line 36, characters 4-11", +(29 === Bs_internalAVLset.maxNull0(u4.data)));

b("File \"bs_poly_set_test.ml\", line 37, characters 4-11", +(1 === Bs_internalAVLset.minNull0(u4.data)));

b("File \"bs_poly_set_test.ml\", line 38, characters 4-11", +(u4 === u5));

b("File \"bs_poly_set_test.ml\", line 39, characters 4-11", Bs_internalAVLset.isEmpty0(u6.data));

eq("File \"bs_poly_set_test.ml\", line 40, characters 6-13", Bs_internalAVLset.length0(u7.data), 3);

b("File \"bs_poly_set_test.ml\", line 41, characters 4-11", 1 - Bs_internalAVLset.isEmpty0(u7.data));

b("File \"bs_poly_set_test.ml\", line 42, characters 4-11", Bs_internalAVLset.isEmpty0(u8.data));

b("File \"bs_poly_set_test.ml\", line 43, characters 4-11", +(u9 === u10));

eq("File \"bs_poly_set_test.ml\", line 44, characters 5-12", Bs_internalAVLset.length0(u10.data), 20001);

eq("File \"bs_poly_set_test.ml\", line 45, characters 5-12", Bs_internalAVLset.length0(u11.data), 19800);

eq("File \"bs_poly_set_test.ml\", line 46, characters 5-12", Bs_internalAVLset.length0(u12.data), 19000);

b("File \"bs_poly_set_test.ml\", line 47, characters 4-11", +(u12 === u13));

eq("File \"bs_poly_set_test.ml\", line 48, characters 5-12", Bs_internalAVLset.length0(u14.data), 10000);

eq("File \"bs_poly_set_test.ml\", line 49, characters 5-12", Bs_internalAVLset.length0(u15.data), 1);

b("File \"bs_poly_set_test.ml\", line 50, characters 4-11", Bs_Set.mem(u15, 20000));

b("File \"bs_poly_set_test.ml\", line 51, characters 4-11", Bs_internalAVLset.isEmpty0(u16.data));

Mt.from_pair_suites("bs_poly_set_test.ml", suites[0]);

var N = 0;

var I = 0;

var A = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.I = I;
exports.A = A;
exports.IntCmp = IntCmp;
/* u0 Not a pure module */
