// GENERATED CODE BY BUCKLESCRIPT VERSION 0.8.0 , PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../caml_builtin_exceptions");

function u() {
  throw Caml_builtin_exceptions.not_found;
}

function f(x) {
  if (typeof x === "number") {
    return 2;
  }
  else {
    switch (x.tag | 0) {
      case 0 : 
          return 1;
      case 1 : 
          throw [
                Caml_builtin_exceptions.assert_failure,
                [
                  "test_trywith.ml",
                  24,
                  9
                ]
              ];
      default:
        return 2;
    }
  }
}

var u1 = "bad character decimal encoding \\";

var v = "bad character decimal encoding \\%c%c%c";

exports.u  = u;
exports.u1 = u1;
exports.v  = v;
exports.f  = f;
/* No side effect */
