'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var v = Caml_builtin_exceptions.not_found;

var u = Caml_builtin_exceptions.not_found;

var s = Caml_builtin_exceptions.end_of_file;

var suites_000 = /* tuple */[
  "not_found_equal",
  (function () {
      return /* Eq */Block.__(0, [
                u,
                v
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "not_found_not_equal_end_of_file",
    (function () {
        return /* Neq */Block.__(1, [
                  u,
                  s
                ]);
      })
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("global_exception_regression_test.ml", suites);

exports.v = v;
exports.u = u;
exports.s = s;
exports.suites = suites;
/*  Not a pure module */
