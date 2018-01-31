'use strict';

var Mt = require("./mt.js");
var Bs_Set = require("../../lib/js/bs_Set.js");
var Bs_List = require("../../lib/js/bs_List.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Bs_SetDict = require("../../lib/js/bs_SetDict.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function t(loc, x) {
  return Mt.throw_suites(test_id, suites, loc, x);
}

var IntCmp = /* module */[/* cmp */Caml_primitive.caml_int_compare];

var u0 = Bs_Set.ofArray(Array_data_util.range(0, 30), IntCmp);

var u1 = Bs_Set.remove(u0, 0);

var u2 = Bs_Set.remove(u1, 0);

var u3 = Bs_Set.remove(u2, 30);

var u4 = Bs_Set.remove(u3, 20);

var r = Array_data_util.randomRange(0, 30);

var u5 = Bs_Set.add(u4, 3);

var u6 = Bs_Set.removeMany(u5, r);

var u7 = Bs_Set.mergeMany(u6, /* int array */[
      0,
      1,
      2,
      0
    ]);

var u8 = Bs_Set.removeMany(u7, /* int array */[
      0,
      1,
      2,
      3
    ]);

var u9 = Bs_Set.mergeMany(u8, Array_data_util.randomRange(0, 20000));

var u10 = Bs_Set.mergeMany(u9, Array_data_util.randomRange(0, 200));

var u11 = Bs_Set.removeMany(u10, Array_data_util.randomRange(0, 200));

var u12 = Bs_Set.removeMany(u11, Array_data_util.randomRange(0, 1000));

var u13 = Bs_Set.removeMany(u12, Array_data_util.randomRange(0, 1000));

var u14 = Bs_Set.removeMany(u13, Array_data_util.randomRange(1000, 10000));

var u15 = Bs_Set.removeMany(u14, Array_data_util.randomRange(10000, 19999));

var u16 = Bs_Set.removeMany(u15, Array_data_util.randomRange(20000, 21000));

b("File \"bs_poly_set_test.ml\", line 33, characters 4-11", +(u0 !== u1));

b("File \"bs_poly_set_test.ml\", line 34, characters 4-11", +(u2 === u1));

eq("File \"bs_poly_set_test.ml\", line 35, characters 5-12", Bs_SetDict.size(u4.data), 28);

b("File \"bs_poly_set_test.ml\", line 36, characters 4-11", +(29 === Bs_SetDict.maxUndefined(u4.data)));

b("File \"bs_poly_set_test.ml\", line 37, characters 4-11", +(1 === Bs_SetDict.minUndefined(u4.data)));

b("File \"bs_poly_set_test.ml\", line 38, characters 4-11", +(u4 === u5));

b("File \"bs_poly_set_test.ml\", line 39, characters 4-11", Bs_SetDict.isEmpty(u6.data));

eq("File \"bs_poly_set_test.ml\", line 40, characters 6-13", Bs_SetDict.size(u7.data), 3);

b("File \"bs_poly_set_test.ml\", line 41, characters 4-11", 1 - Bs_SetDict.isEmpty(u7.data));

b("File \"bs_poly_set_test.ml\", line 42, characters 4-11", Bs_SetDict.isEmpty(u8.data));

b("File \"bs_poly_set_test.ml\", line 45, characters 4-11", Bs_Set.has(u10, 20));

b("File \"bs_poly_set_test.ml\", line 46, characters 4-11", Bs_Set.has(u10, 21));

eq("File \"bs_poly_set_test.ml\", line 47, characters 5-12", Bs_SetDict.size(u10.data), 20001);

eq("File \"bs_poly_set_test.ml\", line 48, characters 5-12", Bs_SetDict.size(u11.data), 19800);

eq("File \"bs_poly_set_test.ml\", line 49, characters 5-12", Bs_SetDict.size(u12.data), 19000);

eq("File \"bs_poly_set_test.ml\", line 51, characters 5-12", Bs_SetDict.size(u13.data), Bs_SetDict.size(u12.data));

eq("File \"bs_poly_set_test.ml\", line 52, characters 5-12", Bs_SetDict.size(u14.data), 10000);

eq("File \"bs_poly_set_test.ml\", line 53, characters 5-12", Bs_SetDict.size(u15.data), 1);

b("File \"bs_poly_set_test.ml\", line 54, characters 4-11", Bs_Set.has(u15, 20000));

b("File \"bs_poly_set_test.ml\", line 55, characters 4-11", 1 - Bs_Set.has(u15, 2000));

b("File \"bs_poly_set_test.ml\", line 56, characters 4-11", Bs_SetDict.isEmpty(u16.data));

var u17 = Bs_Set.ofArray(Array_data_util.randomRange(0, 100), IntCmp);

var u18 = Bs_Set.ofArray(Array_data_util.randomRange(59, 200), IntCmp);

var u19 = Bs_Set.union(u17, u18);

var u20 = Bs_Set.ofArray(Array_data_util.randomRange(0, 200), IntCmp);

b("File \"bs_poly_set_test.ml\", line 61, characters 4-11", Bs_Set.eq(u19, u20));

var u21 = Bs_Set.intersect(u17, u18);

eq("File \"bs_poly_set_test.ml\", line 63, characters 5-12", Bs_SetDict.toArray(u21.data), Array_data_util.range(59, 100));

var u22 = Bs_Set.diff(u17, u18);

eq("File \"bs_poly_set_test.ml\", line 65, characters 5-12", Bs_SetDict.toArray(u22.data), Array_data_util.range(0, 58));

var u23 = Bs_Set.diff(u18, u17);

var u24 = Bs_Set.union(u18, u17);

b("File \"bs_poly_set_test.ml\", line 68, characters 4-11", Bs_Set.eq(u24, u19));

eq("File \"bs_poly_set_test.ml\", line 69, characters 5-12", Bs_SetDict.toArray(u23.data), Array_data_util.range(101, 200));

b("File \"bs_poly_set_test.ml\", line 70, characters 4-11", Bs_Set.subset(u23, u18));

b("File \"bs_poly_set_test.ml\", line 71, characters 4-11", 1 - Bs_Set.subset(u18, u23));

b("File \"bs_poly_set_test.ml\", line 72, characters 4-11", Bs_Set.subset(u22, u17));

b("File \"bs_poly_set_test.ml\", line 73, characters 4-11", Bs_Set.subset(u21, u17) && Bs_Set.subset(u21, u18));

b("File \"bs_poly_set_test.ml\", line 74, characters 4-11", +(47 === Bs_Set.getUndefined(u22, 47)));

b("File \"bs_poly_set_test.ml\", line 75, characters 4-11", Caml_obj.caml_equal(/* Some */[47], Bs_Set.get(u22, 47)));

b("File \"bs_poly_set_test.ml\", line 76, characters 4-11", +(Bs_Set.getUndefined(u22, 59) === undefined));

b("File \"bs_poly_set_test.ml\", line 77, characters 4-11", +(/* None */0 === Bs_Set.get(u22, 59)));

var u25 = Bs_Set.add(u22, 59);

eq("File \"bs_poly_set_test.ml\", line 79, characters 5-12", Bs_SetDict.size(u25.data), 60);

var m = {
  cmp: IntCmp[/* cmp */0],
  data: Bs_SetDict.empty
};

b("File \"bs_poly_set_test.ml\", line 80, characters 4-11", +(Bs_SetDict.minimum(m.data) === /* None */0));

var m$1 = {
  cmp: IntCmp[/* cmp */0],
  data: Bs_SetDict.empty
};

b("File \"bs_poly_set_test.ml\", line 81, characters 4-11", +(Bs_SetDict.maximum(m$1.data) === /* None */0));

var m$2 = {
  cmp: IntCmp[/* cmp */0],
  data: Bs_SetDict.empty
};

b("File \"bs_poly_set_test.ml\", line 82, characters 4-11", +(Bs_SetDict.minUndefined(m$2.data) === undefined));

var m$3 = {
  cmp: IntCmp[/* cmp */0],
  data: Bs_SetDict.empty
};

b("File \"bs_poly_set_test.ml\", line 83, characters 4-11", +(Bs_SetDict.maxUndefined(m$3.data) === undefined));

function testIterToList(xs) {
  var v = [/* [] */0];
  Bs_SetDict.forEach(xs.data, (function (x) {
          v[0] = /* :: */[
            x,
            v[0]
          ];
          return /* () */0;
        }));
  return Bs_List.reverse(v[0]);
}

var u0$1 = Bs_Set.ofArray(Array_data_util.randomRange(0, 20), IntCmp);

var u1$1 = Bs_Set.remove(u0$1, 17);

var u2$1 = Bs_Set.add(u1$1, 33);

b("File \"bs_poly_set_test.ml\", line 94, characters 4-11", Bs_List.every2(testIterToList(u0$1), Bs_List.makeBy(21, (function (i) {
                return i;
              })), (function (x, y) {
            return +(x === y);
          })));

b("File \"bs_poly_set_test.ml\", line 95, characters 4-11", Bs_List.every2(testIterToList(u0$1), Bs_SetDict.toList(u0$1.data), (function (x, y) {
            return +(x === y);
          })));

function f(x) {
  return +(x === 17);
}

b("File \"bs_poly_set_test.ml\", line 96, characters 4-11", Bs_SetDict.some(u0$1.data, f));

function f$1(x) {
  return +(x === 17);
}

b("File \"bs_poly_set_test.ml\", line 97, characters 4-11", 1 - Bs_SetDict.some(u1$1.data, f$1));

function f$2(x) {
  return +(x < 24);
}

b("File \"bs_poly_set_test.ml\", line 98, characters 4-11", Bs_SetDict.every(u0$1.data, f$2));

function f$3(x) {
  return +(x < 24);
}

b("File \"bs_poly_set_test.ml\", line 99, characters 4-11", 1 - Bs_SetDict.every(u2$1.data, f$3));

b("File \"bs_poly_set_test.ml\", line 100, characters 4-11", +(Bs_Set.cmp(u1$1, u0$1) < 0));

b("File \"bs_poly_set_test.ml\", line 101, characters 4-11", +(Bs_Set.cmp(u0$1, u1$1) > 0));

var a0 = Bs_Set.ofArray(Array_data_util.randomRange(0, 1000), IntCmp);

var a1 = Bs_Set.keepBy(a0, (function (x) {
        return +(x % 2 === 0);
      }));

var a2 = Bs_Set.keepBy(a0, (function (x) {
        return +(x % 2 !== 0);
      }));

var match = Bs_Set.partition(a0, (function (x) {
        return +(x % 2 === 0);
      }));

var a4 = match[1];

var a3 = match[0];

b("File \"bs_poly_set_test.ml\", line 111, characters 4-11", Bs_Set.eq(a1, a3));

b("File \"bs_poly_set_test.ml\", line 112, characters 4-11", Bs_Set.eq(a2, a4));

eq("File \"bs_poly_set_test.ml\", line 113, characters 5-12", Bs_Set.getExn(a0, 3), 3);

eq("File \"bs_poly_set_test.ml\", line 114, characters 5-12", Bs_Set.getExn(a0, 4), 4);

t("File \"bs_poly_set_test.ml\", line 115, characters 4-11", (function () {
        Bs_Set.getExn(a0, 1002);
        return /* () */0;
      }));

t("File \"bs_poly_set_test.ml\", line 116, characters 4-11", (function () {
        Bs_Set.getExn(a0, -1);
        return /* () */0;
      }));

eq("File \"bs_poly_set_test.ml\", line 117, characters 5-12", Bs_SetDict.size(a0.data), 1001);

b("File \"bs_poly_set_test.ml\", line 118, characters 4-11", 1 - Bs_SetDict.isEmpty(a0.data));

var match$1 = Bs_Set.split(a0, 200);

var match$2 = match$1[0];

b("File \"bs_poly_set_test.ml\", line 120, characters 4-11", match$1[1]);

eq("File \"bs_poly_set_test.ml\", line 121, characters 5-12", Bs_SetDict.toArray(match$2[0].data), Bs_Array.makeBy(200, (function (i) {
            return i;
          })));

eq("File \"bs_poly_set_test.ml\", line 122, characters 5-12", Bs_SetDict.toList(match$2[1].data), Bs_List.makeBy(800, (function (i) {
            return i + 201 | 0;
          })));

var a7 = Bs_Set.remove(a0, 200);

var match$3 = Bs_Set.split(a7, 200);

var match$4 = match$3[0];

var a9 = match$4[1];

var a8 = match$4[0];

b("File \"bs_poly_set_test.ml\", line 125, characters 4-11", 1 - match$3[1]);

eq("File \"bs_poly_set_test.ml\", line 126, characters 5-12", Bs_SetDict.toArray(a8.data), Bs_Array.makeBy(200, (function (i) {
            return i;
          })));

eq("File \"bs_poly_set_test.ml\", line 127, characters 5-12", Bs_SetDict.toList(a9.data), Bs_List.makeBy(800, (function (i) {
            return i + 201 | 0;
          })));

eq("File \"bs_poly_set_test.ml\", line 128, characters 5-12", Bs_SetDict.minimum(a8.data), /* Some */[0]);

eq("File \"bs_poly_set_test.ml\", line 129, characters 5-12", Bs_SetDict.minimum(a9.data), /* Some */[201]);

b("File \"bs_poly_set_test.ml\", line 130, characters 4-11", Bs_List.every(/* :: */[
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
            return Bs_SetDict.checkInvariantInternal(x.data);
          })));

var a = Bs_Set.ofArray(/* int array */[], IntCmp);

var m$4 = Bs_Set.keepBy(a, (function (x) {
        return +(x % 2 === 0);
      }));

b("File \"bs_poly_set_test.ml\", line 135, characters 4-11", Bs_SetDict.isEmpty(m$4.data));

var match$5 = Bs_Set.split({
      cmp: IntCmp[/* cmp */0],
      data: Bs_SetDict.empty
    }, 0);

var match$6 = match$5[0];

b("File \"bs_poly_set_test.ml\", line 139, characters 4-11", Bs_SetDict.isEmpty(match$6[0].data));

b("File \"bs_poly_set_test.ml\", line 140, characters 4-11", Bs_SetDict.isEmpty(match$6[1].data));

b("File \"bs_poly_set_test.ml\", line 141, characters 4-11", 1 - match$5[1]);

Mt.from_pair_suites("bs_poly_set_test.ml", suites[0]);

var N = 0;

var I = 0;

var A = 0;

var L = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.t = t;
exports.N = N;
exports.I = I;
exports.A = A;
exports.IntCmp = IntCmp;
exports.L = L;
exports.testIterToList = testIterToList;
/* u0 Not a pure module */
