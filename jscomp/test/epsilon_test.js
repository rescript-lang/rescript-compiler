'use strict';

var Mt = require("./mt.js");
var Pervasives = require("../../lib/js/pervasives.js");

var v = (Number.EPSILON?Number.EPSILON:2.220446049250313e-16);

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "epsilon",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: Pervasives.epsilon_float,
                Arg1: v
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "raw_epsilon",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: 2.220446049250313e-16,
                  Arg1: v
                };
        })
    ],
    Arg1: "[]"
  }
};

Mt.from_pair_suites("Epsilon_test", suites);

exports.v = v;
exports.suites = suites;
/* v Not a pure module */
