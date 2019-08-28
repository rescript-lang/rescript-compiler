'use strict';

var Mt = require("./mt.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var match = /* array */[1];

if (match.length !== 1) {
  throw [
        Caml_builtin_exceptions.match_failure,
        /* tuple */[
          "gpr_3595_test.ml",
          9,
          4
        ]
      ];
}

var a = match[0];

var x = 1;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.a = a;
exports.x = x;
/*  Not a pure module */
