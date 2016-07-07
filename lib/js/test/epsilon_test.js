// GENERATED CODE BY BUCKLESCRIPT VERSION 0.7.1 , PLEASE EDIT WITH CARE
'use strict';

var Pervasives = require("../pervasives");
var Mt         = require("./mt");
var Block      = require("../block");

var v = (Number.EPSILON?Number.EPSILON:2.220446049250313e-16);

var suites_000 = /* tuple */[
  "epsilon",
  function () {
    return /* Eq */Block.__(0, [
              Pervasives.epsilon_float,
              v
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "raw_epsilon",
    function () {
      return /* Eq */Block.__(0, [
                2.220446049250313e-16,
                v
              ]);
    }
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("epsilon_test.ml", suites);

exports.v      = v;
exports.suites = suites;
/* v Not a pure module */
