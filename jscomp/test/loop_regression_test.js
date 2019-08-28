'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");

function f(param) {
  var v = /* record */{
    contents: 0
  };
  var acc = /* record */{
    contents: 0
  };
  var n = 10;
  while(true) {
    if (v.contents > n) {
      return acc.contents;
    } else {
      acc.contents = acc.contents + v.contents | 0;
      Pervasives.incr(v);
      continue ;
    }
  };
}

var suites_000 = /* tuple */[
  "sum",
  (function (param) {
      return /* Eq */Block.__(0, [
                55,
                f(/* () */0)
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
