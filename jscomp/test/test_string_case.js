'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(param) {
  switch (param) {
    case "abcd" : 
        return 0;
    case "bcde" : 
        return 1;
    default:
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "test/test_string_case.ml",
              4,
              9
            ]
          ];
  }
}

exports.f = f;
/* No side effect */
