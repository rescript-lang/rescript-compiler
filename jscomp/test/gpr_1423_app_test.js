'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Gpr_1423_nav = require("./gpr_1423_nav.js");

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

function foo(f) {
  console.log(Curry._2(f, "a1", /* () */0));
  return /* () */0;
}

foo((function (param) {
        return (function (param$1) {
            return Gpr_1423_nav.busted(param, "a2", param$1);
          });
      }));

function foo2(f) {
  return Curry._2(f, "a1", /* () */0);
}

eq("File \"gpr_1423_app_test.ml\", line 18, characters 7-14", Curry._1((function (param) {
              return (function (param$1) {
                  return Gpr_1423_nav.busted(param, "a2", param$1);
                });
            })("a1"), /* () */0), "a1a2");

Mt.from_pair_suites("gpr_1423_app_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.foo = foo;
exports.foo2 = foo2;
/*  Not a pure module */
