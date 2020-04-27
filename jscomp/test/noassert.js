'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(param) {
  throw {
        CamlExt: Caml_builtin_exceptions.assert_failure,
        _1: /* tuple */[
          "noassert.ml",
          5,
          11
        ]
      };
}

function h(param) {
  
}

exports.f = f;
exports.h = h;
/* No side effect */
