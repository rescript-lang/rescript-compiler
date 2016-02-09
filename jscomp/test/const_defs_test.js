// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_exceptions = require("../runtime/caml_exceptions");

var u = 3;

function f() {
  throw [
        0,
        Caml_exceptions.Invalid_argument,
        "hi"
      ];
}

exports.u = u;
exports.f = f;
/* No side effect */
