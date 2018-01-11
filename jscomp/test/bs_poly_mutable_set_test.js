'use strict';

var Mt = require("./mt.js");
var Bs_SetM = require("../../lib/js/bs_SetM.js");
var Caml_array = require("../../lib/js/caml_array.js");
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

var u = Bs_SetM.ofArray(IntCmp, Array_data_util.range(0, 30));

b("File \"bs_poly_mutable_set_test.ml\", line 15, characters 4-11", Bs_SetM.removeCheck(u, 0));

b("File \"bs_poly_mutable_set_test.ml\", line 16, characters 4-11", 1 - Bs_SetM.removeCheck(u, 0));

b("File \"bs_poly_mutable_set_test.ml\", line 17, characters 4-11", Bs_SetM.removeCheck(u, 30));

b("File \"bs_poly_mutable_set_test.ml\", line 18, characters 4-11", Bs_SetM.removeCheck(u, 20));

eq("File \"bs_poly_mutable_set_test.ml\", line 19, characters 5-12", Bs_internalAVLset.length0(u.data), 28);

var r = Array_data_util.randomRange(0, 30);

b("File \"bs_poly_mutable_set_test.ml\", line 21, characters 4-11", +(29 === Bs_internalAVLset.maxNull0(u.data)));

b("File \"bs_poly_mutable_set_test.ml\", line 22, characters 4-11", +(1 === Bs_internalAVLset.minNull0(u.data)));

Bs_SetM.addOnly(u, 3);

for(var i = 0 ,i_finish = r.length - 1 | 0; i <= i_finish; ++i){
  Bs_SetM.removeOnly(u, Caml_array.caml_array_get(r, i));
}

b("File \"bs_poly_mutable_set_test.ml\", line 27, characters 4-11", Bs_internalAVLset.isEmpty0(u.data));

Bs_SetM.addOnly(u, 0);

Bs_SetM.addOnly(u, 1);

Bs_SetM.addOnly(u, 2);

Bs_SetM.addOnly(u, 0);

eq("File \"bs_poly_mutable_set_test.ml\", line 32, characters 5-12", Bs_internalAVLset.length0(u.data), 3);

b("File \"bs_poly_mutable_set_test.ml\", line 33, characters 4-11", 1 - Bs_internalAVLset.isEmpty0(u.data));

for(var i$1 = 0; i$1 <= 3; ++i$1){
  Bs_SetM.removeOnly(u, i$1);
}

b("File \"bs_poly_mutable_set_test.ml\", line 37, characters 4-11", Bs_internalAVLset.isEmpty0(u.data));

Bs_SetM.addArrayOnly(u, Array_data_util.randomRange(0, 20000));

Bs_SetM.addArrayOnly(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_poly_mutable_set_test.ml\", line 40, characters 5-12", Bs_internalAVLset.length0(u.data), 20001);

Bs_SetM.removeArrayOnly(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_poly_mutable_set_test.ml\", line 42, characters 5-12", Bs_internalAVLset.length0(u.data), 19800);

Bs_SetM.removeArrayOnly(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_poly_mutable_set_test.ml\", line 44, characters 5-12", Bs_internalAVLset.length0(u.data), 19000);

Bs_SetM.removeArrayOnly(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_poly_mutable_set_test.ml\", line 46, characters 5-12", Bs_internalAVLset.length0(u.data), 19000);

Bs_SetM.removeArrayOnly(u, Array_data_util.randomRange(1000, 10000));

eq("File \"bs_poly_mutable_set_test.ml\", line 48, characters 5-12", Bs_internalAVLset.length0(u.data), 10000);

Bs_SetM.removeArrayOnly(u, Array_data_util.randomRange(10000, 19999));

eq("File \"bs_poly_mutable_set_test.ml\", line 50, characters 5-12", Bs_internalAVLset.length0(u.data), 1);

b("File \"bs_poly_mutable_set_test.ml\", line 51, characters 4-11", Bs_SetM.mem(u, 20000));

function f(param) {
  return Bs_SetM.ofArray(IntCmp, param);
}

var aa = f(Array_data_util.randomRange(0, 100));

var bb = f(Array_data_util.randomRange(40, 120));

var cc = Bs_SetM.union(aa, bb);

b("File \"bs_poly_mutable_set_test.ml\", line 61, characters 4-11", Bs_SetM.eq(cc, f(Array_data_util.randomRange(0, 120))));

b("File \"bs_poly_mutable_set_test.ml\", line 63, characters 4-11", Bs_SetM.eq(Bs_SetM.union(f(Array_data_util.randomRange(0, 20)), f(Array_data_util.randomRange(21, 40))), f(Array_data_util.randomRange(0, 40))));

Mt.from_pair_suites("bs_poly_mutable_set_test.ml", suites[0]);

var N = 0;

var I = 0;

var A = 0;

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
exports.$plus$plus = $plus$plus;
exports.f = f;
exports.$eq$tilde = $eq$tilde;
/* u Not a pure module */
