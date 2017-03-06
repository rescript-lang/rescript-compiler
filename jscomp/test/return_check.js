'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");

function test(dom) {
  var elem = dom.getElementById("haha");
  if (elem !== null) {
    console.log(elem);
    return 2;
  }
  else {
    return 1;
  }
}

function f(xs, i) {
  var match = xs[i];
  if (match !== undefined) {
    return match;
  }
  else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "return_check.ml",
            29,
            14
          ]
        ];
  }
}

exports.test = test;
exports.f    = f;
/* No side effect */
