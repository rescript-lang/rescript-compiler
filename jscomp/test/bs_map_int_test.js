'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_MapInt = require("../../lib/js/bs_MapInt.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
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
      loc + (" id " + test_id[0]),
      (function () {
          return /* Ok */Block.__(4, [v]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

var v = Bs_Array.init(1000000, (function (i) {
        return /* tuple */[
                i,
                i
              ];
      }));

Bs_Array.shuffleInPlace(v);

var u = Bs_MapInt.ofArray(v);

b("File \"bs_map_int_test.ml\", line 20, characters 4-11", Bs_MapInt.checkInvariant(u));

var firstHalf = Bs_Array.sub(v, 0, 2000);

Bs_Array.foldLeft(firstHalf, u, (function (acc, param) {
        return Bs_MapInt.remove(param[0], acc);
      }));

b("File \"bs_map_int_test.ml\", line 24, characters 4-11", Bs_MapInt.checkInvariant(u));

Mt.from_pair_suites("bs_map_int_test.ml", suites[0]);

var N = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
/* v Not a pure module */
