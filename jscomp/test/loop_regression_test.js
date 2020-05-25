'use strict';

var Mt = require("./mt.js");

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

var suites_0 = [
  "sum",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: 55,
              _1: f(undefined)
            };
    })
];

var suites = /* :: */{
  _0: suites_0,
  _1: /* [] */0
};

Mt.from_pair_suites("Loop_regression_test", suites);

exports.f = f;
exports.suites = suites;
/*  Not a pure module */
