'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var match = [1];

if (match.length !== 1) {
  throw {
        RE_EXN_ID: "Match_failure",
        _1: /* tuple */[
          "gpr_3595_test.ml",
          9,
          4
        ]
      };
}

var a = match[0];

var x = 1;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.a = a;
exports.x = x;
/*  Not a pure module */
