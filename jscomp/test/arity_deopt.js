'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

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

function f0(x, y, z) {
  return (x + y | 0) + z | 0;
}

function f1(x) {
  return (function (y, z) {
      return (x + y | 0) + z | 0;
    });
}

function f2(x, y) {
  return (function (z) {
      return (x + y | 0) + z | 0;
    });
}

function f3(x) {
  return (function (y, z) {
      return (x + y | 0) + z | 0;
    });
}

eq("File \"arity_deopt.ml\", line 45, characters 7-14", 6, f0(1, 2, 3));

eq("File \"arity_deopt.ml\", line 46, characters 11-18", 6, (function (y, z) {
          return (1 + y | 0) + z | 0;
        })(2, 3));

eq("File \"arity_deopt.ml\", line 47, characters 15-22", 6, Curry._1(f2(1, 2), 3));

eq("File \"arity_deopt.ml\", line 48, characters 15-22", 6, (function (y, z) {
          return (1 + y | 0) + z | 0;
        })(2, 3));

Mt.from_pair_suites("arity_deopt.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f0 = f0;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
/*  Not a pure module */
