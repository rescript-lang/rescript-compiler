'use strict';

var Mt = require("./mt.js");

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

function hi (cb){
    cb ();
    return 0;
}
;

var xs = {
  contents: /* [] */0
};

hi(function () {
      xs.contents = {
        hd: undefined,
        tl: xs.contents
      };
      
    });

hi(function () {
      xs.contents = {
        hd: undefined,
        tl: xs.contents
      };
      
    });

eq("File \"bs_auto_uncurry_test.ml\", line 27, characters 7-14", xs.contents, {
      hd: undefined,
      tl: {
        hd: undefined,
        tl: /* [] */0
      }
    });

eq("File \"bs_auto_uncurry_test.ml\", line 33, characters 7-14", [
        1,
        2,
        3
      ].map(function (x) {
          return x + 1 | 0;
        }), [
      2,
      3,
      4
    ]);

eq("File \"bs_auto_uncurry_test.ml\", line 36, characters 7-14", [
        1,
        2,
        3
      ].map(function (x) {
          return x + 1 | 0;
        }), [
      2,
      3,
      4
    ]);

eq("File \"bs_auto_uncurry_test.ml\", line 40, characters 7-14", [
        1,
        2,
        3
      ].reduce((function (prim, prim$1) {
            return prim + prim$1 | 0;
          }), 0), 6);

eq("File \"bs_auto_uncurry_test.ml\", line 44, characters 7-14", [
        1,
        2,
        3
      ].reduce((function (x, y, i) {
            return (x + y | 0) + i | 0;
          }), 0), 9);

eq("File \"bs_auto_uncurry_test.ml\", line 48, characters 7-14", [
        1,
        2,
        3
      ].some(function (x) {
          return x < 1;
        }), false);

eq("File \"bs_auto_uncurry_test.ml\", line 52, characters 7-14", [
        1,
        2,
        3
      ].every(function (x) {
          return x > 0;
        }), true);

Mt.from_pair_suites("Bs_auto_uncurry_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
