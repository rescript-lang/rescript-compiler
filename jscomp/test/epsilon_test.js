'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");

var v = (Number.EPSILON?Number.EPSILON:2.220446049250313e-16);

var suites_000 = /* tuple */[
  "epsilon",
  (function (param) {
      return /* Eq */Block.__(0, [
                Pervasives.epsilon_float,
                v
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "raw_epsilon",
    (function (param) {
        return /* Eq */Block.__(0, [
                  2.220446049250313e-16,
                  v
                ]);
      })
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("Epsilon_test", suites);

exports.v = v;
exports.suites = suites;
/* v Not a pure module */
