// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt");

function f() {
  var v = [0];
  var acc = [0];
  var n = 10;
  while(true) {
    if (v[0] > n) {
      return acc[0];
    }
    else {
      acc[0] = acc[0] + v[0] | 0;
      v[0] = v[0] + 1 | 0;
      continue ;
      
    }
  };
}

var suites_000 = /* tuple */[
  "sum",
  function () {
    return /* Eq */{
            0: 55,
            1: f(/* () */0),
            length: 2,
            tag: 0
          };
  }
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("loop_regression_test.ml", suites);

exports.f      = f;
exports.suites = suites;
/*  Not a pure module */
