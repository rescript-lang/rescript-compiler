'use strict';

var Mt = require("./mt.js");

function f(param) {
  var v = /* record */[/* contents */0];
  var acc = /* record */[/* contents */0];
  var n = 10;
  while(true) {
    if (v[0] > n) {
      return acc[0];
    } else {
      acc[0] = acc[0] + v[0] | 0;
      v[0] = v[0] + 1 | 0;
      continue ;
    }
  };
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "sum",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: 55,
                Arg1: f(/* () */0)
              };
      })
  ],
  Arg1: "[]"
};

Mt.from_pair_suites("Loop_regression_test", suites);

exports.f = f;
exports.suites = suites;
/*  Not a pure module */
