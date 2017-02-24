'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

eq('File "bs_auto_uncurry_test.ml", line 15, characters 7-14', /* int array */[
        1,
        2,
        3
      ].map(function (x) {
          return x + 1 | 0;
        }), /* int array */[
      2,
      3,
      4
    ]);

eq('File "bs_auto_uncurry_test.ml", line 18, characters 7-14', /* int array */[
        1,
        2,
        3
      ].map(function (x) {
          return x + 1 | 0;
        }), /* int array */[
      2,
      3,
      4
    ]);

eq('File "bs_auto_uncurry_test.ml", line 22, characters 7-14', /* int array */[
        1,
        2,
        3
      ].reduce(function (prim, prim$1) {
          return prim + prim$1 | 0;
        }, 0), 6);

eq('File "bs_auto_uncurry_test.ml", line 26, characters 7-14', /* int array */[
        1,
        2,
        3
      ].reduce(function (x, y, i) {
          return (x + y | 0) + i | 0;
        }, 0), 9);

eq('File "bs_auto_uncurry_test.ml", line 30, characters 7-14', /* int array */[
        1,
        2,
        3
      ].some(function (x) {
          return +(x < 1);
        }), false);

eq('File "bs_auto_uncurry_test.ml", line 34, characters 7-14', /* int array */[
        1,
        2,
        3
      ].every(function (x) {
          return +(x > 0);
        }), true);

Mt.from_pair_suites("bs_auto_uncurry_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
