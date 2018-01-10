'use strict';

var Const_defs = require("./const_defs.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var u = Const_defs.v ? 3 : 4;

function f() {
  throw [
        Caml_builtin_exceptions.invalid_argument,
        "hi"
      ];
}

exports.u = u;
exports.f = f;
/* No side effect */
