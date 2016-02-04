// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_exceptions = require("../runtime/caml_exceptions");

function u() {
  throw Caml_exceptions.Not_found;
}

function f(x) {
  if (typeof x === "number") {
    return 2;
  }
  else {
    switch (x[0]) {
      case 0 : 
          return 1;
      case 1 : 
          throw [
                0,
                Caml_exceptions.Assert_failure,
                [
                  0,
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
