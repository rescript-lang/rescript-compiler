'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var u = 3;

function f(param) {
  throw {
        CamlExt: Caml_builtin_exceptions.invalid_argument,
        _1: "hi"
      };
}

exports.u = u;
exports.f = f;
/* No side effect */
