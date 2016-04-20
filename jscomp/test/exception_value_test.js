// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");

function f() {
  throw Caml_builtin_exceptions.not_found;
}

function assert_f(x) {
  if (x > 3) {
    return 0;
  }
  else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "exception_value_test.ml",
            7,
            2
          ]
        ];
  }
}

exports.f        = f;
exports.assert_f = assert_f;
/* No side effect */
