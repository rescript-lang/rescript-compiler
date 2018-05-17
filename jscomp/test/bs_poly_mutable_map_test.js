'use strict';

var Mt = require("./mt.js");
var Belt_Id = require("../../lib/js/belt_Id.js");
var Belt_Set = require("../../lib/js/belt_Set.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");
var Belt_MutableMap = require("../../lib/js/belt_MutableMap.js");
var Belt_internalAVLtree = require("../../lib/js/belt_internalAVLtree.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, v) {
  return Mt.bool_suites(test_id, suites, loc, v);
}

var Icmp = Belt_Id.comparable(Caml_primitive.caml_int_compare);

function f(x) {
  return Belt_MutableMap.fromArray(x, Icmp);
}

function ff(x) {
  return Belt_Set.fromArray(x, Icmp);
}

function randomRange(i, j) {
  return Belt_Array.map(Array_data_util.randomRange(i, j), (function (x) {
                return /* tuple */[
                        x,
                        x
                      ];
              }));
}

var x = randomRange(0, 10);

var a0 = Belt_MutableMap.fromArray(x, Icmp);

Belt_MutableMap.set(a0, 3, 33);

eq("File \"bs_poly_mutable_map_test.ml\", line 27, characters 7-14", Belt_MutableMap.getExn(a0, 3), 33);

Belt_MutableMap.removeMany(a0, /* array */[
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

eq("File \"bs_poly_mutable_map_test.ml\", line 29, characters 7-14", Belt_internalAVLtree.keysToArray(a0.data), /* array */[
      9,
      10
    ]);

Belt_MutableMap.removeMany(a0, Array_data_util.randomRange(0, 100));

b("File \"bs_poly_mutable_map_test.ml\", line 31, characters 6-13", Belt_internalAVLtree.isEmpty(a0.data));

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
/* Icmp Not a pure module */
