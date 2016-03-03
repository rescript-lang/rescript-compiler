// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_curry              = require("../runtime/caml_curry");

function f(g, x) {
  try {
    return Caml_curry.app1(g, x);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return 3;
    }
    else {
      throw exn;
    }
  }
}

exports.f = f;
/* No side effect */
