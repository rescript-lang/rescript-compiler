'use strict';

var Mt = require("./mt.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var v = Caml_builtin_exceptions.not_found;

var u = Caml_builtin_exceptions.not_found;

var s = Caml_builtin_exceptions.end_of_file;

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "not_found_equal",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: u,
                Arg1: v
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "not_found_not_equal_end_of_file",
      (function (param) {
          return /* constructor */{
                  tag: "Neq",
                  Arg0: u,
                  Arg1: s
                };
        })
    ],
    Arg1: "[]"
  }
};

Mt.from_pair_suites("Global_exception_regression_test", suites);

exports.v = v;
exports.u = u;
exports.s = s;
exports.suites = suites;
/*  Not a pure module */
