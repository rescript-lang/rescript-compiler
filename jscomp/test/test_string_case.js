// GENERATED CODE BY BUCKLESCRIPT VERSION 0.3 , PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");

function f(param) {
  switch (param) {
    case "abcd" : 
        return 0;
    case "bcde" : 
        return 1;
    default:
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "test_string_case.ml",
              4,
              9
            ]
          ];
  }
}

exports.f = f;
/* No side effect */
