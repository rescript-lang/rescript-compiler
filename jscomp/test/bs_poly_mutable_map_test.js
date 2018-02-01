'use strict';

var Mt = require("./mt.js");
var Bs_Set = require("../../lib/js/bs_Set.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_MutableMap = require("../../lib/js/bs_MutableMap.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");
var Bs_internalAVLtree = require("../../lib/js/bs_internalAVLtree.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, v) {
  return Mt.bool_suites(test_id, suites, loc, v);
}

var Icmp = /* module */[/* cmp */Caml_primitive.caml_int_compare];

function f(x) {
  return Bs_MutableMap.ofArray(x, Icmp);
}

function ff(x) {
  return Bs_Set.ofArray(x, Icmp);
}

function randomRange(i, j) {
  return Bs_Array.map(Array_data_util.randomRange(i, j), (function (x) {
                return /* tuple */[
                        x,
                        x
                      ];
              }));
}

var x = randomRange(0, 10);

var a0 = Bs_MutableMap.ofArray(x, Icmp);

Bs_MutableMap.set(a0, 3, 33);

eq("File \"bs_poly_mutable_map_test.ml\", line 28, characters 7-14", Bs_MutableMap.getExn(a0, 3), 33);

Bs_MutableMap.removeMany(a0, /* array */[
      7,
      8,
      0,
      1,
      3,
      2,
      4,
      922,
      4,
      5,
      6
    ]);

eq("File \"bs_poly_mutable_map_test.ml\", line 30, characters 7-14", Bs_internalAVLtree.keysToArray(a0.data), /* int array */[
      9,
      10
    ]);

Bs_MutableMap.removeMany(a0, Array_data_util.randomRange(0, 100));

b("File \"bs_poly_mutable_map_test.ml\", line 32, characters 6-13", Bs_internalAVLtree.isEmpty(a0.data));

Mt.from_pair_suites("bs_poly_mutable_map_test.ml", suites[0]);

var M = 0;

var N = 0;

var A = 0;

var I = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.Icmp = Icmp;
exports.M = M;
exports.N = N;
exports.A = A;
exports.I = I;
exports.f = f;
exports.ff = ff;
exports.randomRange = randomRange;
/* x Not a pure module */
