// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");

var u = 3;

function f() {
  throw [
        Caml_builtin_exceptions.invalid_argument,
        "hi"
      ];
}

exports.u = u;
exports.f = f;
/* No side effect */
