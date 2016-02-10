// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Mt                      = require("./mt");

var v = Caml_builtin_exceptions.Not_found;

var u = Caml_builtin_exceptions.Not_found;

var s = Caml_builtin_exceptions.End_of_file;

var suites_001 = [
  /* tuple */0,
  "not_found_equal",
  function () {
    return [
            /* Eq */0,
            u,
            v
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "not_found_not_equal_end_of_file",
    function () {
      return [
              /* Neq */1,
              u,
              s
            ];
    }
  ],
  /* [] */0
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("global_exception_regression_test.ml", suites);

exports.v      = v;
exports.u      = u;
exports.s      = s;
exports.suites = suites;
/*  Not a pure module */
