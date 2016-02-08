// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt");

function f() {
  var v = [
    0,
    0
  ];
  var acc = [
    0,
    0
  ];
  var n = 10;
  while(true) {
    if (v[1] > n) {
      return acc[1];
    }
    else {
      acc[1] += v[1];
      ++ v[1];
      continue ;
      
    }
  };
}

var suites_001 = [
  /* tuple */0,
  "sum",
  function () {
    return [
            /* Eq */0,
            55,
            f(/* () */0)
          ];
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_pair_suites("loop_regression_test.ml", suites);

exports.f      = f;
exports.suites = suites;
/*  Not a pure module */
