'use strict';

var Mt = require("./mt.js");
var Belt_Id = require("../../lib/js/belt_Id.js");
var Belt_Set = require("../../lib/js/belt_Set.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_SetDict = require("../../lib/js/belt_SetDict.js");
var Belt_SortArray = require("../../lib/js/belt_SortArray.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function t(loc, x) {
  return Mt.throw_suites(test_id, suites, loc, x);
}

var IntCmp = Belt_Id.comparable(Caml_primitive.caml_int_compare);

var u0 = Belt_Set.fromArray(Array_data_util.range(0, 30), IntCmp);

var u1 = Belt_Set.remove(u0, 0);

var u2 = Belt_Set.remove(u1, 0);

var u3 = Belt_Set.remove(u2, 30);

var u4 = Belt_Set.remove(u3, 20);

var r = Array_data_util.randomRange(0, 30);

var u5 = Belt_Set.add(u4, 3);

var u6 = Belt_Set.removeMany(u5, r);

var u7 = Belt_Set.mergeMany(u6, /* array */[
      0,
      1,
      2,
      0
    ]);

var u8 = Belt_Set.removeMany(u7, /* array */[
      0,
      1,
      2,
      3
    ]);

var u9 = Belt_Set.mergeMany(u8, Array_data_util.randomRange(0, 20000));

var u10 = Belt_Set.mergeMany(u9, Array_data_util.randomRange(0, 200));

var u11 = Belt_Set.removeMany(u10, Array_data_util.randomRange(0, 200));

var u12 = Belt_Set.removeMany(u11, Array_data_util.randomRange(0, 1000));

var u13 = Belt_Set.removeMany(u12, Array_data_util.randomRange(0, 1000));

var u14 = Belt_Set.removeMany(u13, Array_data_util.randomRange(1000, 10000));

var u15 = Belt_Set.removeMany(u14, Array_data_util.randomRange(10000, 19999));

var u16 = Belt_Set.removeMany(u15, Array_data_util.randomRange(20000, 21000));

b("File \"bs_poly_set_test.ml\", line 35, characters 4-11", u0 !== u1);

b("File \"bs_poly_set_test.ml\", line 36, characters 4-11", u2 === u1);

eq("File \"bs_poly_set_test.ml\", line 37, characters 5-12", Belt_SetDict.size(u4.data), 28);

b("File \"bs_poly_set_test.ml\", line 38, characters 4-11", 29 === Belt_SetDict.maxUndefined(u4.data));

b("File \"bs_poly_set_test.ml\", line 39, characters 4-11", 1 === Belt_SetDict.minUndefined(u4.data));

b("File \"bs_poly_set_test.ml\", line 40, characters 4-11", u4 === u5);

b("File \"bs_poly_set_test.ml\", line 41, characters 4-11", Belt_SetDict.isEmpty(u6.data));

eq("File \"bs_poly_set_test.ml\", line 42, characters 6-13", Belt_SetDict.size(u7.data), 3);

b("File \"bs_poly_set_test.ml\", line 43, characters 4-11", !Belt_SetDict.isEmpty(u7.data));

b("File \"bs_poly_set_test.ml\", line 44, characters 4-11", Belt_SetDict.isEmpty(u8.data));

b("File \"bs_poly_set_test.ml\", line 47, characters 4-11", Belt_Set.has(u10, 20));

b("File \"bs_poly_set_test.ml\", line 48, characters 4-11", Belt_Set.has(u10, 21));

eq("File \"bs_poly_set_test.ml\", line 49, characters 5-12", Belt_SetDict.size(u10.data), 20001);

eq("File \"bs_poly_set_test.ml\", line 50, characters 5-12", Belt_SetDict.size(u11.data), 19800);

eq("File \"bs_poly_set_test.ml\", line 51, characters 5-12", Belt_SetDict.size(u12.data), 19000);

eq("File \"bs_poly_set_test.ml\", line 53, characters 5-12", Belt_SetDict.size(u13.data), Belt_SetDict.size(u12.data));

eq("File \"bs_poly_set_test.ml\", line 54, characters 5-12", Belt_SetDict.size(u14.data), 10000);

eq("File \"bs_poly_set_test.ml\", line 55, characters 5-12", Belt_SetDict.size(u15.data), 1);

b("File \"bs_poly_set_test.ml\", line 56, characters 4-11", Belt_Set.has(u15, 20000));

b("File \"bs_poly_set_test.ml\", line 57, characters 4-11", !Belt_Set.has(u15, 2000));

b("File \"bs_poly_set_test.ml\", line 58, characters 4-11", Belt_SetDict.isEmpty(u16.data));

var u17 = Belt_Set.fromArray(Array_data_util.randomRange(0, 100), IntCmp);

var u18 = Belt_Set.fromArray(Array_data_util.randomRange(59, 200), IntCmp);

var u19 = Belt_Set.union(u17, u18);

var u20 = Belt_Set.fromArray(Array_data_util.randomRange(0, 200), IntCmp);

var u21 = Belt_Set.intersect(u17, u18);

var u22 = Belt_Set.diff(u17, u18);

var u23 = Belt_Set.diff(u18, u17);

var u24 = Belt_Set.union(u18, u17);

var u25 = Belt_Set.add(u22, 59);

var u26 = Belt_Set.add({
      cmp: IntCmp[/* cmp */0],
      data: Belt_SetDict.empty
    }, 3);

var ss = Belt_Array.makeByAndShuffle(100, (function (i) {
        return (i << 1);
      }));

var u27 = Belt_Set.fromArray(ss, IntCmp);

var u28 = Belt_Set.union(u27, u26);

var u29 = Belt_Set.union(u26, u27);

b("File \"bs_poly_set_test.ml\", line 72, characters 4-11", Belt_Set.eq(u28, u29));

b("File \"bs_poly_set_test.ml\", line 73, characters 4-11", Caml_obj.caml_equal(Belt_SetDict.toArray(u29.data), Belt_SortArray.stableSortBy(Belt_Array.concat(ss, /* array */[3]), Caml_primitive.caml_int_compare)));

b("File \"bs_poly_set_test.ml\", line 74, characters 4-11", Belt_Set.eq(u19, u20));

eq("File \"bs_poly_set_test.ml\", line 75, characters 5-12", Belt_SetDict.toArray(u21.data), Array_data_util.range(59, 100));

eq("File \"bs_poly_set_test.ml\", line 76, characters 5-12", Belt_SetDict.toArray(u22.data), Array_data_util.range(0, 58));

b("File \"bs_poly_set_test.ml\", line 77, characters 4-11", Belt_Set.eq(u24, u19));

eq("File \"bs_poly_set_test.ml\", line 78, characters 5-12", Belt_SetDict.toArray(u23.data), Array_data_util.range(101, 200));

b("File \"bs_poly_set_test.ml\", line 79, characters 4-11", Belt_Set.subset(u23, u18));

b("File \"bs_poly_set_test.ml\", line 80, characters 4-11", !Belt_Set.subset(u18, u23));

b("File \"bs_poly_set_test.ml\", line 81, characters 4-11", Belt_Set.subset(u22, u17));

b("File \"bs_poly_set_test.ml\", line 82, characters 4-11", Belt_Set.subset(u21, u17) && Belt_Set.subset(u21, u18));

b("File \"bs_poly_set_test.ml\", line 83, characters 4-11", 47 === Belt_Set.getUndefined(u22, 47));

b("File \"bs_poly_set_test.ml\", line 84, characters 4-11", Caml_obj.caml_equal(47, Belt_Set.get(u22, 47)));

b("File \"bs_poly_set_test.ml\", line 85, characters 4-11", Belt_Set.getUndefined(u22, 59) === undefined);

b("File \"bs_poly_set_test.ml\", line 86, characters 4-11", undefined === Belt_Set.get(u22, 59));

eq("File \"bs_poly_set_test.ml\", line 88, characters 5-12", Belt_SetDict.size(u25.data), 60);

var m = {
  cmp: IntCmp[/* cmp */0],
  data: Belt_SetDict.empty
};

b("File \"bs_poly_set_test.ml\", line 89, characters 4-11", Belt_SetDict.minimum(m.data) === undefined);

var m$1 = {
  cmp: IntCmp[/* cmp */0],
  data: Belt_SetDict.empty
};

b("File \"bs_poly_set_test.ml\", line 90, characters 4-11", Belt_SetDict.maximum(m$1.data) === undefined);

var m$2 = {
  cmp: IntCmp[/* cmp */0],
  data: Belt_SetDict.empty
};

b("File \"bs_poly_set_test.ml\", line 91, characters 4-11", Belt_SetDict.minUndefined(m$2.data) === undefined);

var m$3 = {
  cmp: IntCmp[/* cmp */0],
  data: Belt_SetDict.empty
};

b("File \"bs_poly_set_test.ml\", line 92, characters 4-11", Belt_SetDict.maxUndefined(m$3.data) === undefined);

function testIterToList(xs) {
  var v = /* record */[/* contents : [] */0];
  Belt_Set.forEach(xs, (function (x) {
          v[0] = /* :: */[
            x,
            v[0]
          ];
          return /* () */0;
        }));
  return Belt_List.reverse(v[0]);
}

function testIterToList2(xs) {
  var v = /* record */[/* contents : [] */0];
  Belt_SetDict.forEach(xs.data, (function (x) {
          v[0] = /* :: */[
            x,
            v[0]
          ];
          return /* () */0;
        }));
  return Belt_List.reverse(v[0]);
}

var u0$1 = Belt_Set.fromArray(Array_data_util.randomRange(0, 20), IntCmp);

var u1$1 = Belt_Set.remove(u0$1, 17);

var u2$1 = Belt_Set.add(u1$1, 33);

b("File \"bs_poly_set_test.ml\", line 109, characters 4-11", Belt_List.every2(testIterToList(u0$1), Belt_List.makeBy(21, (function (i) {
                return i;
              })), (function (x, y) {
            return x === y;
          })));

b("File \"bs_poly_set_test.ml\", line 110, characters 4-11", Belt_List.every2(testIterToList2(u0$1), Belt_List.makeBy(21, (function (i) {
                return i;
              })), (function (x, y) {
            return x === y;
          })));

b("File \"bs_poly_set_test.ml\", line 111, characters 4-11", Belt_List.every2(testIterToList(u0$1), Belt_SetDict.toList(u0$1.data), (function (x, y) {
            return x === y;
          })));

b("File \"bs_poly_set_test.ml\", line 112, characters 4-11", Belt_Set.some(u0$1, (function (x) {
            return x === 17;
          })));

b("File \"bs_poly_set_test.ml\", line 113, characters 4-11", !Belt_Set.some(u1$1, (function (x) {
            return x === 17;
          })));

b("File \"bs_poly_set_test.ml\", line 114, characters 4-11", Belt_Set.every(u0$1, (function (x) {
            return x < 24;
          })));

b("File \"bs_poly_set_test.ml\", line 115, characters 4-11", Belt_SetDict.every(u0$1.data, (function (x) {
            return x < 24;
          })));

b("File \"bs_poly_set_test.ml\", line 116, characters 4-11", !Belt_Set.every(u2$1, (function (x) {
            return x < 24;
          })));

b("File \"bs_poly_set_test.ml\", line 117, characters 4-11", !Belt_Set.every(Belt_Set.fromArray(/* array */[
              1,
              2,
              3
            ], IntCmp), (function (x) {
            return x === 2;
          })));

b("File \"bs_poly_set_test.ml\", line 118, characters 4-11", Belt_Set.cmp(u1$1, u0$1) < 0);

b("File \"bs_poly_set_test.ml\", line 119, characters 4-11", Belt_Set.cmp(u0$1, u1$1) > 0);

var a0 = Belt_Set.fromArray(Array_data_util.randomRange(0, 1000), IntCmp);

var a1 = Belt_Set.keep(a0, (function (x) {
        return x % 2 === 0;
      }));

var a2 = Belt_Set.keep(a0, (function (x) {
        return x % 2 !== 0;
      }));

var match = Belt_Set.partition(a0, (function (x) {
        return x % 2 === 0;
      }));

var a4 = match[1];

var a3 = match[0];

b("File \"bs_poly_set_test.ml\", line 129, characters 4-11", Belt_Set.eq(a1, a3));

b("File \"bs_poly_set_test.ml\", line 130, characters 4-11", Belt_Set.eq(a2, a4));

eq("File \"bs_poly_set_test.ml\", line 131, characters 5-12", Belt_Set.getExn(a0, 3), 3);

eq("File \"bs_poly_set_test.ml\", line 132, characters 5-12", Belt_Set.getExn(a0, 4), 4);

t("File \"bs_poly_set_test.ml\", line 133, characters 4-11", (function (param) {
        Belt_Set.getExn(a0, 1002);
        return /* () */0;
      }));

t("File \"bs_poly_set_test.ml\", line 134, characters 4-11", (function (param) {
        Belt_Set.getExn(a0, -1);
        return /* () */0;
      }));

eq("File \"bs_poly_set_test.ml\", line 135, characters 5-12", Belt_SetDict.size(a0.data), 1001);

b("File \"bs_poly_set_test.ml\", line 136, characters 4-11", !Belt_SetDict.isEmpty(a0.data));

var match$1 = Belt_Set.split(a0, 200);

var match$2 = match$1[0];

b("File \"bs_poly_set_test.ml\", line 138, characters 4-11", match$1[1]);

eq("File \"bs_poly_set_test.ml\", line 139, characters 5-12", Belt_SetDict.toArray(match$2[0].data), Belt_Array.makeBy(200, (function (i) {
            return i;
          })));

eq("File \"bs_poly_set_test.ml\", line 140, characters 5-12", Belt_SetDict.toList(match$2[1].data), Belt_List.makeBy(800, (function (i) {
            return i + 201 | 0;
          })));

var a7 = Belt_Set.remove(a0, 200);

var match$3 = Belt_Set.split(a7, 200);

var match$4 = match$3[0];

var a9 = match$4[1];

var a8 = match$4[0];

b("File \"bs_poly_set_test.ml\", line 143, characters 4-11", !match$3[1]);

eq("File \"bs_poly_set_test.ml\", line 144, characters 5-12", Belt_SetDict.toArray(a8.data), Belt_Array.makeBy(200, (function (i) {
            return i;
          })));

eq("File \"bs_poly_set_test.ml\", line 145, characters 5-12", Belt_SetDict.toList(a9.data), Belt_List.makeBy(800, (function (i) {
            return i + 201 | 0;
          })));

eq("File \"bs_poly_set_test.ml\", line 146, characters 5-12", Belt_SetDict.minimum(a8.data), 0);

eq("File \"bs_poly_set_test.ml\", line 147, characters 5-12", Belt_SetDict.minimum(a9.data), 201);

Belt_List.forEach(/* :: */[
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
        return Belt_SetDict.checkInvariantInternal(x.data);
      }));

var a = Belt_Set.fromArray(/* array */[], IntCmp);

var m$4 = Belt_Set.keep(a, (function (x) {
        return x % 2 === 0;
      }));

b("File \"bs_poly_set_test.ml\", line 153, characters 4-11", Belt_SetDict.isEmpty(m$4.data));

var match$5 = Belt_Set.split({
      cmp: IntCmp[/* cmp */0],
      data: Belt_SetDict.empty
    }, 0);

var match$6 = match$5[0];

b("File \"bs_poly_set_test.ml\", line 157, characters 4-11", Belt_SetDict.isEmpty(match$6[0].data));

b("File \"bs_poly_set_test.ml\", line 158, characters 4-11", Belt_SetDict.isEmpty(match$6[1].data));

b("File \"bs_poly_set_test.ml\", line 159, characters 4-11", !match$5[1]);

Mt.from_pair_suites("Bs_poly_set_test", suites[0]);

var N = 0;

var D = 0;

var I = 0;

var A = 0;

var S = 0;

var L = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.t = t;
exports.N = N;
exports.D = D;
exports.I = I;
exports.A = A;
exports.S = S;
exports.IntCmp = IntCmp;
exports.L = L;
exports.testIterToList = testIterToList;
exports.testIterToList2 = testIterToList2;
/* IntCmp Not a pure module */
