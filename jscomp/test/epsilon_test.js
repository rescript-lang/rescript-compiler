'use strict';

var Mt         = require("./mt");
var Block      = require("../../lib/js/block");
var Pervasives = require("../../lib/js/pervasives");

var v = (Number.EPSILON?Number.EPSILON:2.220446049250313e-16);

var suites_000 = /* tuple */[
  "epsilon",function () {
    return /* Eq */Block.__(0, [
              Pervasives.epsilon_float,v
            ]);
  }
];

var suites_001 = /* Nested :: */[
  /* tuple */[
    "raw_epsilon",function () {
      return /* Eq */Block.__(0, [
                2.220446049250313e-16,v
              ]);
    }
  ],/* [] */0
];

var suites = /* Nested :: */[
  suites_000,suites_001
];

Mt.from_pair_suites("epsilon_test.ml", suites);

exports.v      = v;
exports.suites = suites;
/* v Not a pure module */
