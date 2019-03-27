'use strict';

var Mt = require("./mt.js");
var Belt_Id = require("../../lib/js/belt_Id.js");
var Belt_Map = require("../../lib/js/belt_Map.js");
var Belt_Set = require("../../lib/js/belt_Set.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_MapDict = require("../../lib/js/belt_MapDict.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Array_data_util = require("./array_data_util.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, v) {
  return Mt.bool_suites(test_id, suites, loc, v);
}

var Icmp = Belt_Id.comparable(Caml_primitive.caml_int_compare);

function mapOfArray(x) {
  return Belt_Map.fromArray(x, Icmp);
}

function setOfArray(x) {
  return Belt_Set.fromArray(x, Icmp);
}

function emptyMap(param) {
  return {
          cmp: Icmp[/* cmp */0],
          data: Belt_MapDict.empty
        };
}

function mergeInter(s1, s2) {
  var m = Belt_Map.merge(s1, s2, (function (k, v1, v2) {
          if (v1 !== undefined && v2 !== undefined) {
            return /* () */0;
          }
          
        }));
  var x = Belt_MapDict.keysToArray(m.data);
  return Belt_Set.fromArray(x, Icmp);
}

function mergeUnion(s1, s2) {
  var m = Belt_Map.merge(s1, s2, (function (k, v1, v2) {
          if (v1 !== undefined || v2 !== undefined) {
            return /* () */0;
          }
          
        }));
  var x = Belt_MapDict.keysToArray(m.data);
  return Belt_Set.fromArray(x, Icmp);
}

function mergeDiff(s1, s2) {
  var m = Belt_Map.merge(s1, s2, (function (k, v1, v2) {
          if (v1 !== undefined && v2 === undefined) {
            return /* () */0;
          }
          
        }));
  var x = Belt_MapDict.keysToArray(m.data);
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

var x = randomRange(0, 100);

var u0 = Belt_Map.fromArray(x, Icmp);

var x$1 = randomRange(30, 120);

var u1 = Belt_Map.fromArray(x$1, Icmp);

var x$2 = Array_data_util.range(30, 100);

b("File \"bs_poly_map_test.ml\", line 48, characters 4-11", Belt_Set.eq(mergeInter(u0, u1), Belt_Set.fromArray(x$2, Icmp)));

var x$3 = Array_data_util.range(0, 120);

b("File \"bs_poly_map_test.ml\", line 49, characters 4-11", Belt_Set.eq(mergeUnion(u0, u1), Belt_Set.fromArray(x$3, Icmp)));

var x$4 = Array_data_util.range(0, 29);

b("File \"bs_poly_map_test.ml\", line 50, characters 4-11", Belt_Set.eq(mergeDiff(u0, u1), Belt_Set.fromArray(x$4, Icmp)));

var x$5 = Array_data_util.range(101, 120);

b("File \"bs_poly_map_test.ml\", line 51, characters 4-11", Belt_Set.eq(mergeDiff(u1, u0), Belt_Set.fromArray(x$5, Icmp)));

var x$6 = randomRange(0, 10);

var a0 = Belt_Map.fromArray(x$6, Icmp);

var a1 = Belt_Map.set(a0, 3, 33);

var a2 = Belt_Map.remove(a1, 3);

var a3 = Belt_Map.update(a2, 3, (function (k) {
        if (k !== undefined) {
          return k + 1 | 0;
        } else {
          return 11;
        }
      }));

var a4 = Belt_Map.update(a2, 3, (function (k) {
        if (k !== undefined) {
          return k + 1 | 0;
        }
        
      }));

var a5 = Belt_Map.remove(a0, 3);

var a6 = Belt_Map.remove(a5, 3);

b("File \"bs_poly_map_test.ml\", line 70, characters 4-11", a5 === a6);

b("File \"bs_poly_map_test.ml\", line 71, characters 4-11", Belt_Map.has(a0, 3));

b("File \"bs_poly_map_test.ml\", line 72, characters 4-11", !Belt_Map.has(a5, 3));

b("File \"bs_poly_map_test.ml\", line 73, characters 4-11", 3 === Belt_Map.getUndefined(a0, 3));

b("File \"bs_poly_map_test.ml\", line 74, characters 4-11", 33 === Belt_Map.getUndefined(a1, 3));

b("File \"bs_poly_map_test.ml\", line 75, characters 4-11", Belt_Map.getUndefined(a2, 3) === undefined);

b("File \"bs_poly_map_test.ml\", line 77, characters 4-11", 11 === Belt_Map.getUndefined(a3, 3));

b("File \"bs_poly_map_test.ml\", line 78, characters 4-11", Belt_Map.getUndefined(a4, 3) === undefined);

var a7 = Belt_Map.removeMany(a0, /* array */[
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

eq("File \"bs_poly_map_test.ml\", line 81, characters 5-12", Belt_MapDict.keysToArray(a7.data), /* array */[
      9,
      10
    ]);

var a8 = Belt_Map.removeMany(a7, Array_data_util.randomRange(0, 100));

b("File \"bs_poly_map_test.ml\", line 83, characters 4-11", Belt_MapDict.isEmpty(a8.data));

var x$7 = randomRange(0, 100);

var u0$1 = Belt_Map.fromArray(x$7, Icmp);

var u1$1 = Belt_Map.set(u0$1, 3, 32);

eq("File \"bs_poly_map_test.ml\", line 90, characters 5-12", Belt_Map.get(u1$1, 3), 32);

eq("File \"bs_poly_map_test.ml\", line 91, characters 5-12", Belt_Map.get(u0$1, 3), 3);

function acc(m, is) {
  return Belt_Array.reduce(is, m, (function (a, i) {
                var m = a;
                var i$1 = i;
                return Belt_Map.update(m, i$1, (function (n) {
                              if (n !== undefined) {
                                return n + 1 | 0;
                              } else {
                                return 1;
                              }
                            }));
              }));
}

var m = {
  cmp: Icmp[/* cmp */0],
  data: Belt_MapDict.empty
};

var m1 = acc(m, Belt_Array.concat(Array_data_util.randomRange(0, 20), Array_data_util.randomRange(10, 30)));

var x$8 = Belt_Array.makeBy(31, (function (i) {
        return /* tuple */[
                i,
                i >= 10 && i <= 20 ? 2 : 1
              ];
      }));

b("File \"bs_poly_map_test.ml\", line 103, characters 4-11", Belt_Map.eq(m1, Belt_Map.fromArray(x$8, Icmp), (function (x, y) {
            return x === y;
          })));

var v0 = {
  cmp: Icmp[/* cmp */0],
  data: Belt_MapDict.empty
};

var v1 = Belt_Map.mergeMany(v0, Belt_Array.map(Array_data_util.randomRange(0, 10000), (function (x) {
            return /* tuple */[
                    x,
                    x
                  ];
          })));

var x$9 = Belt_Array.map(Array_data_util.randomRange(0, 10000), (function (x) {
        return /* tuple */[
                x,
                x
              ];
      }));

var v2 = Belt_Map.fromArray(x$9, Icmp);

b("File \"bs_poly_map_test.ml\", line 117, characters 4-11", Belt_Map.eq(v1, v2, (function (x, y) {
            return x === y;
          })));

function inc(x) {
  if (x !== undefined) {
    return x + 1 | 0;
  } else {
    return 0;
  }
}

var v3 = Belt_Map.update(v1, 10, inc);

var v4 = Belt_Map.update(v3, -10, inc);

var match = Belt_Map.split(v3, 5000);

var pres = match[1];

var match$1 = match[0];

var match$2 = Belt_Map.get(v3, 10);

b("File \"bs_poly_map_test.ml\", line 126, characters 4-11", match$2 !== undefined ? match$2 === 11 : false);

var match$3 = Belt_Map.get(v3, -10);

b("File \"bs_poly_map_test.ml\", line 127, characters 4-11", match$3 === undefined);

var match$4 = Belt_Map.get(v4, -10);

b("File \"bs_poly_map_test.ml\", line 128, characters 4-11", match$4 !== undefined ? match$4 === 0 : false);

var map = Belt_Map.remove({
      cmp: Icmp[/* cmp */0],
      data: Belt_MapDict.empty
    }, 0);

b("File \"bs_poly_map_test.ml\", line 129, characters 4-11", Belt_MapDict.isEmpty(map.data));

var map$1 = Belt_Map.removeMany({
      cmp: Icmp[/* cmp */0],
      data: Belt_MapDict.empty
    }, /* array */[0]);

b("File \"bs_poly_map_test.ml\", line 130, characters 4-11", Belt_MapDict.isEmpty(map$1.data));

b("File \"bs_poly_map_test.ml\", line 131, characters 4-11", pres !== undefined ? pres === 5000 : false);

b("File \"bs_poly_map_test.ml\", line 132, characters 4-11", Belt_Array.eq(Belt_MapDict.keysToArray(match$1[0].data), Belt_Array.makeBy(5000, (function (i) {
                return i;
              })), (function (prim, prim$1) {
            return prim === prim$1;
          })));

b("File \"bs_poly_map_test.ml\", line 133, characters 4-11", Belt_Array.eq(Belt_MapDict.keysToArray(match$1[1].data), Belt_Array.makeBy(5000, (function (i) {
                return 5001 + i | 0;
              })), (function (prim, prim$1) {
            return prim === prim$1;
          })));

var v7 = Belt_Map.remove(v3, 5000);

var match$5 = Belt_Map.split(v7, 5000);

var match$6 = match$5[0];

b("File \"bs_poly_map_test.ml\", line 137, characters 4-11", match$5[1] === undefined);

b("File \"bs_poly_map_test.ml\", line 138, characters 4-11", Belt_Array.eq(Belt_MapDict.keysToArray(match$6[0].data), Belt_Array.makeBy(5000, (function (i) {
                return i;
              })), (function (prim, prim$1) {
            return prim === prim$1;
          })));

b("File \"bs_poly_map_test.ml\", line 139, characters 4-11", Belt_Array.eq(Belt_MapDict.keysToArray(match$6[1].data), Belt_Array.makeBy(5000, (function (i) {
                return 5001 + i | 0;
              })), (function (prim, prim$1) {
            return prim === prim$1;
          })));

Mt.from_pair_suites("Bs_poly_map_test", suites[0]);

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
exports.mapOfArray = mapOfArray;
exports.setOfArray = setOfArray;
exports.emptyMap = emptyMap;
exports.mergeInter = mergeInter;
exports.mergeUnion = mergeUnion;
exports.mergeDiff = mergeDiff;
exports.randomRange = randomRange;
exports.acc = acc;
/* Icmp Not a pure module */
