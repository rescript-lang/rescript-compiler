'use strict';

var Mt = require("./mt.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_MapInt = require("../../lib/js/belt_MapInt.js");
var Belt_SetInt = require("../../lib/js/belt_SetInt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

function b(loc, v) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Ok */4,
                  _0: v
                };
        })
    ],
    tl: suites.contents
  };
  
}

var mapOfArray = Belt_MapInt.fromArray;

var setOfArray = Belt_SetInt.fromArray;

function emptyMap(param) {
  
}

var v = Belt_Array.makeByAndShuffle(1000000, (function (i) {
        return [
                i,
                i
              ];
      }));

var u = Belt_MapInt.fromArray(v);

Belt_MapInt.checkInvariantInternal(u);

var firstHalf = Belt_Array.slice(v, 0, 2000);

var xx = Belt_Array.reduce(firstHalf, u, (function (acc, param) {
        return Belt_MapInt.remove(acc, param[0]);
      }));

Belt_MapInt.checkInvariantInternal(u);

Belt_MapInt.checkInvariantInternal(xx);

Mt.from_pair_suites("Bs_map_test", suites.contents);

var M;

var N;

var A;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.M = M;
exports.N = N;
exports.A = A;
exports.mapOfArray = mapOfArray;
exports.setOfArray = setOfArray;
exports.emptyMap = emptyMap;
/* v Not a pure module */
