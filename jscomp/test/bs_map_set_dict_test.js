'use strict';

var Mt = require("./mt.js");
var Belt_Id = require("../../lib/js/belt_Id.js");
var Belt_Map = require("../../lib/js/belt_Map.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_MapInt = require("../../lib/js/belt_MapInt.js");
var Belt_MapDict = require("../../lib/js/belt_MapDict.js");
var Belt_SetDict = require("../../lib/js/belt_SetDict.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, v) {
  return Mt.bool_suites(test_id, suites, loc, v);
}

var Icmp = Belt_Id.comparable(Caml_primitive.caml_int_compare);

var Icmp2 = Belt_Id.comparable(Caml_primitive.caml_int_compare);

var m0 = {
  cmp: Icmp[/* cmp */0],
  data: Belt_MapDict.empty
};

var I2 = Belt_Id.comparable((function (x, y) {
        return Caml_primitive.caml_int_compare(y, x);
      }));

var m = {
  cmp: Icmp2[/* cmp */0],
  data: Belt_MapDict.empty
};

var m2 = {
  cmp: I2[/* cmp */0],
  data: Belt_MapDict.empty
};

var data = m.data;

Belt_Map.getId(m2);

var m_dict = Belt_Map.getId(m);

for(var i = 0; i <= 100000; ++i){
  data = Belt_MapDict.set(data, i, i, m_dict[/* cmp */0]);
}

var data$1 = data;

var newm = {
  cmp: m_dict[/* cmp */0],
  data: data$1
};

console.log(newm);

var m11 = Belt_MapDict.set(Belt_MapDict.empty, 1, 1, Icmp[/* cmp */0]);

console.log(m11);

var v = {
  cmp: Icmp2[/* cmp */0],
  data: Belt_SetDict.empty
};

var m_dict$1 = Belt_Map.getId(m);

var cmp = m_dict$1[/* cmp */0];

var data$2 = v.data;

for(var i$1 = 0; i$1 <= 100000; ++i$1){
  data$2 = Belt_SetDict.add(data$2, i$1, cmp);
}

console.log(data$2);

function f(param) {
  return Belt_Map.ofArray(param, Icmp);
}

function $eq$tilde(a, b) {
  return (function (param) {
      return Belt_Map.eq(a, b, param);
    });
}

var u0 = f(Belt_Array.map(Array_data_util.randomRange(0, 39), (function (x) {
            return /* tuple */[
                    x,
                    x
                  ];
          })));

var u1 = Belt_Map.set(u0, 39, 120);

b("File \"bs_map_set_dict_test.ml\", line 77, characters 4-11", Belt_Array.every2(Belt_MapDict.toArray(u0.data), Belt_Array.map(Array_data_util.range(0, 39), (function (x) {
                return /* tuple */[
                        x,
                        x
                      ];
              })), (function (param, param$1) {
            if (param[0] === param$1[0]) {
              return param[1] === param$1[1];
            } else {
              return false;
            }
          })));

b("File \"bs_map_set_dict_test.ml\", line 82, characters 4-11", Belt_List.every2(Belt_MapDict.toList(u0.data), Belt_List.ofArray(Belt_Array.map(Array_data_util.range(0, 39), (function (x) {
                    return /* tuple */[
                            x,
                            x
                          ];
                  }))), (function (param, param$1) {
            if (param[0] === param$1[0]) {
              return param[1] === param$1[1];
            } else {
              return false;
            }
          })));

eq("File \"bs_map_set_dict_test.ml\", line 87, characters 5-12", Belt_Map.get(u0, 39), /* Some */[39]);

eq("File \"bs_map_set_dict_test.ml\", line 88, characters 5-12", Belt_Map.get(u1, 39), /* Some */[120]);

var u = f(Belt_Array.makeByAndShuffle(10000, (function (x) {
            return /* tuple */[
                    x,
                    x
                  ];
          })));

eq("File \"bs_map_set_dict_test.ml\", line 94, characters 4-11", Belt_Array.makeBy(10000, (function (x) {
            return /* tuple */[
                    x,
                    x
                  ];
          })), Belt_MapDict.toArray(u.data));

Mt.from_pair_suites("bs_map_set_dict_test.ml", suites[0]);

var M = 0;

var MI = 0;

var I = 0;

var A = 0;

var L = 0;

var vv = Belt_MapInt.empty;

var vv2 = Belt_MapInt.empty;

var Md0 = 0;

var ISet = 0;

var S0 = 0;

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
exports.Md0 = Md0;
exports.ISet = ISet;
exports.S0 = S0;
exports.f = f;
exports.$eq$tilde = $eq$tilde;
/* Icmp Not a pure module */
