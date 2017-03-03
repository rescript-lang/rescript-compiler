'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");

function f(xs, i) {
  var match = xs[i];
  if (match) {
    return match[0];
  }
  else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "return_check.ml",
            11,
            14
          ]
        ];
  }
}

exports.f = f;
/* No side effect */
