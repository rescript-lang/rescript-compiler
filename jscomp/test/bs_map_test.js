'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_MapInt = require("../../lib/js/belt_MapInt.js");
var Belt_SetInt = require("../../lib/js/belt_SetInt.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function b(loc, v) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function () {
          return /* Ok */Block.__(4, [v]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

var mapOfArray = Belt_MapInt.fromArray;

var setOfArray = Belt_SetInt.fromArray;

function emptyMap() {
  return Belt_MapInt.empty;
}

var v = Belt_Array.makeByAndShuffle(1000000, (function (i) {
        return /* tuple */[
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

Mt.from_pair_suites("bs_map_test.ml", suites[0]);

var M = 0;

var N = 0;

var A = 0;

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
