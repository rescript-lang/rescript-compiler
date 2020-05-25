'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Gpr_1423_nav = require("./gpr_1423_nav.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

function foo(f) {
  console.log(Curry._2(f, "a1", undefined));
  
}

foo(function (param) {
      return function (param$1) {
        return Gpr_1423_nav.busted(param, "a2", param$1);
      };
    });

function foo2(f) {
  return Curry._2(f, "a1", undefined);
}

eq("File \"gpr_1423_app_test.ml\", line 18, characters 7-14", "a1a2", "a1a2");

Mt.from_pair_suites("Gpr_1423_app_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.foo = foo;
exports.foo2 = foo2;
/*  Not a pure module */
