'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

function f(param) {
  var v = {
    contents: 0
  };
  var acc = {
    contents: 0
  };
  var n = 10;
  while(true) {
    if (v.contents > n) {
      return acc.contents;
    }
    acc.contents = acc.contents + v.contents | 0;
    v.contents = v.contents + 1 | 0;
    continue ;
  };
}

var suites_000 = /* tuple */[
  "sum",
  (function (param) {
      return /* Eq */Block.__(0, [
                55,
                f(undefined)
              ]);
    })
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("Loop_regression_test", suites);

exports.f = f;
exports.suites = suites;
/*  Not a pure module */
