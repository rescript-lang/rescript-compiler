'use strict';

var Mt = require("./mt.js");
var Bs_Map = require("../../lib/js/bs_Map.js");
var Bs_Set = require("../../lib/js/bs_Set.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
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
  return Bs_Map.ofArray(Icmp, x);
}

function ff(x) {
  return Bs_Set.ofArray(Icmp, x);
}

function mergeInter(s1, s2) {
  var m = Bs_Map.merge(s1, s2, (function (_, v1, v2) {
          if (v1 && v2) {
            return /* Some */[/* () */0];
          } else {
            return /* None */0;
          }
        }));
  var x = Bs_internalAVLtree.keysToArray0(m.data);
  return Bs_Set.ofArray(Icmp, x);
}

function mergeUnion(s1, s2) {
  var m = Bs_Map.merge(s1, s2, (function (_, v1, v2) {
          if (v1) {
            return /* Some */[/* () */0];
          } else if (v2) {
            return /* Some */[/* () */0];
          } else {
            return /* None */0;
          }
        }));
  var x = Bs_internalAVLtree.keysToArray0(m.data);
  return Bs_Set.ofArray(Icmp, x);
}

function mergeDiff(s1, s2) {
  var m = Bs_Map.merge(s1, s2, (function (_, v1, v2) {
          if (v1 && !v2) {
            return /* Some */[/* () */0];
          } else {
            return /* None */0;
          }
        }));
  var x = Bs_internalAVLtree.keysToArray0(m.data);
  return Bs_Set.ofArray(Icmp, x);
}

function randomRange(i, j) {
  return Bs_Array.map(Array_data_util.randomRange(i, j), (function (x) {
                return /* tuple */[
                        x,
                        x
                      ];
              }));
}

var x = randomRange(0, 100);

var u0 = Bs_Map.ofArray(Icmp, x);

var x$1 = randomRange(30, 120);

var u1 = Bs_Map.ofArray(Icmp, x$1);

var x$2 = Array_data_util.range(30, 100);

b("File \"bs_poly_map_test.ml\", line 47, characters 4-11", Bs_Set.eq(mergeInter(u0, u1), Bs_Set.ofArray(Icmp, x$2)));

var x$3 = Array_data_util.range(0, 120);

b("File \"bs_poly_map_test.ml\", line 48, characters 4-11", Bs_Set.eq(mergeUnion(u0, u1), Bs_Set.ofArray(Icmp, x$3)));

var x$4 = Array_data_util.range(0, 29);

b("File \"bs_poly_map_test.ml\", line 49, characters 4-11", Bs_Set.eq(mergeDiff(u0, u1), Bs_Set.ofArray(Icmp, x$4)));

var x$5 = Array_data_util.range(101, 120);

b("File \"bs_poly_map_test.ml\", line 50, characters 4-11", Bs_Set.eq(mergeDiff(u1, u0), Bs_Set.ofArray(Icmp, x$5)));

var x$6 = randomRange(0, 10);

var a0 = Bs_Map.ofArray(Icmp, x$6);

var a1 = Bs_Map.update(a0, 3, 33);

var a2 = Bs_Map.remove(a1, 3);

b("File \"bs_poly_map_test.ml\", line 57, characters 4-11", +(3 === Bs_Map.findNull(a0, 3)));

b("File \"bs_poly_map_test.ml\", line 58, characters 4-11", +(33 === Bs_Map.findNull(a1, 3)));

b("File \"bs_poly_map_test.ml\", line 59, characters 4-11", +(Bs_Map.findNull(a2, 3) === null));

var a3 = Bs_Map.updateWithOpt(a2, 3, (function (k) {
        if (k) {
          return /* Some */[k[0] + 1 | 0];
        } else {
          return /* Some */[11];
        }
      }));

var a4 = Bs_Map.updateWithOpt(a2, 3, (function (k) {
        if (k) {
          return /* Some */[k[0] + 1 | 0];
        } else {
          return /* None */0;
        }
      }));

b("File \"bs_poly_map_test.ml\", line 70, characters 4-11", +(11 === Bs_Map.findNull(a3, 3)));

b("File \"bs_poly_map_test.ml\", line 71, characters 4-11", +(Bs_Map.findNull(a4, 3) === null));

Mt.from_pair_suites("bs_poly_map_test.ml", suites[0]);

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
exports.mergeInter = mergeInter;
exports.mergeUnion = mergeUnion;
exports.mergeDiff = mergeDiff;
exports.randomRange = randomRange;
/* x Not a pure module */
