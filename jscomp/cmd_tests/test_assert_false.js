// GENERATED CODE BY BUCKLESCRIPT VERSION 0.6.2 , PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions");

function f(x) {
  if (x) {
    var match = x[1];
    if (match) {
      if (match[1]) {
        return 3;
      }
      else {
        return 2;
      }
    }
    else {
      return /* impossible branch */0;
    }
  }
  else {
    throw Caml_builtin_exceptions.not_found;
  }
}

exports.f = f;
/* No side effect */
