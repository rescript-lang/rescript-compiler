'use strict';

var Mt = require("./mt.js");
var Bs_Map = require("../../lib/js/bs_Map.js");
var Bs_Set = require("../../lib/js/bs_Set.js");
var Bs_List = require("../../lib/js/bs_List.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_MapInt = require("../../lib/js/bs_MapInt.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");
var Bs_internalAVLset = require("../../lib/js/bs_internalAVLset.js");
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

var cmp = Caml_primitive.caml_int_compare;

var Icmp2 = /* module */[/* cmp */cmp];

var m0 = {
  dict: Icmp,
  data: Bs_internalAVLtree.empty0
};

function cmp$1(x, y) {
  return Caml_primitive.caml_int_compare(y, x);
}

var I2 = /* module */[/* cmp */cmp$1];

var m = {
  dict: Icmp2,
  data: Bs_internalAVLtree.empty0
};

var m2 = {
  dict: I2,
  data: Bs_internalAVLtree.empty0
};

var data = m.data;

m2.dict;

var m_dict = m.dict;

for(var i = 0; i <= 100000; ++i){
  data = Bs_Map.set0(data, i, i, m_dict[/* cmp */0]);
}

var newm = {
  dict: m_dict,
  data: data
};

console.log(newm);

var m11 = Bs_Map.set0(Bs_Map.empty0, 1, 1, Icmp[/* cmp */0]);

console.log(m11);

var v = {
  dict: Icmp2,
  data: Bs_internalAVLset.empty0
};

var m_dict$1 = m.dict;

var cmp$2 = m_dict$1[/* cmp */0];

var data$1 = v.data;

for(var i$1 = 0; i$1 <= 100000; ++i$1){
  data$1 = Bs_Set.add0(data$1, i$1, cmp$2);
}

console.log(data$1);

function f(param) {
  return Bs_Map.ofArray(param, Icmp);
}

function $eq$tilde(a, b) {
  return (function (param) {
      return Bs_Map.eq(a, b, param);
    });
}

var u0 = f(Bs_Array.map(Array_data_util.randomRange(0, 39), (function (x) {
            return /* tuple */[
                    x,
                    x
                  ];
          })));

var u1 = Bs_Map.set(u0, 39, 120);

b("File \"bs_map_test.ml\", line 83, characters 4-11", Bs_Array.every2(Bs_internalAVLtree.toArray0(u0.data), Bs_Array.map(Array_data_util.range(0, 39), (function (x) {
                return /* tuple */[
                        x,
                        x
                      ];
              })), (function (param, param$1) {
            if (param[0] === param$1[0]) {
              return +(param[1] === param$1[1]);
            } else {
              return /* false */0;
            }
          })));

b("File \"bs_map_test.ml\", line 88, characters 4-11", Bs_List.every2(Bs_internalAVLtree.toList0(u0.data), Bs_Array.toList(Bs_Array.map(Array_data_util.range(0, 39), (function (x) {
                    return /* tuple */[
                            x,
                            x
                          ];
                  }))), (function (param, param$1) {
            if (param[0] === param$1[0]) {
              return +(param[1] === param$1[1]);
            } else {
              return /* false */0;
            }
          })));

eq("File \"bs_map_test.ml\", line 93, characters 5-12", Bs_Map.get(u0, 39), /* Some */[39]);

eq("File \"bs_map_test.ml\", line 94, characters 5-12", Bs_Map.get(u1, 39), /* Some */[120]);

var u = f(Bs_Array.makeByAndShuffle(10000, (function (x) {
            return /* tuple */[
                    x,
                    x
                  ];
          })));

eq("File \"bs_map_test.ml\", line 100, characters 4-11", Bs_Array.makeBy(10000, (function (x) {
            return /* tuple */[
                    x,
                    x
                  ];
          })), Bs_internalAVLtree.toArray0(u.data));

Mt.from_pair_suites("bs_map_test.ml", suites[0]);

var M = 0;

var MI = 0;

var I = 0;

var A = 0;

var L = 0;

var vv = Bs_MapInt.empty;

var vv2 = Bs_MapInt.empty;

var ISet = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.Icmp = Icmp;
exports.Icmp2 = Icmp2;
exports.M = M;
exports.MI = MI;
exports.I = I;
exports.A = A;
exports.L = L;
exports.m0 = m0;
exports.I2 = I2;
exports.m = m;
exports.m2 = m2;
exports.vv = vv;
exports.vv2 = vv2;
exports.ISet = ISet;
exports.f = f;
exports.$eq$tilde = $eq$tilde;
/* data Not a pure module */
