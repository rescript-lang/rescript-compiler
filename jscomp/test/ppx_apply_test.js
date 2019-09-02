'use strict';

var Mt = require("./mt.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

var u = 3;

function nullary() {
  return 3;
}

function unary(a) {
  return a + 3 | 0;
}

var xx = unary(3);

eq("File \"ppx_apply_test.ml\", line 17, characters 5-12", u, 3);

Mt.from_pair_suites("Ppx_apply_test", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
exports.nullary = nullary;
exports.unary = unary;
exports.xx = xx;
/* u Not a pure module */
