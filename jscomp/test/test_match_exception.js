// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Caml_exceptions = require("../runtime/caml_exceptions");

function f(g, x) {
  try {
    return g(x);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      return 3;
    }
    else {
      throw exn;
    }
  }
}

exports.f = f;
/* No side effect */
