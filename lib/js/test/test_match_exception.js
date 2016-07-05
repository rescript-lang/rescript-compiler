// GENERATED CODE BY BUCKLESCRIPT VERSION 0.7.0 , PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../caml_builtin_exceptions");
var Curry                   = require("../curry");

function f(g, x) {
  try {
    return Curry._1(g, x);
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
