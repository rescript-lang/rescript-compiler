'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f0(x) {
  var tmp;
  if (x > 3) {
    tmp = (function (x) {
        return x + 1 | 0;
      });
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
  return tmp(3);
}

function f1(x) {
  throw Caml_builtin_exceptions.not_found;
  return Curry._1(undefined, x);
}

function f3(x) {
  var tmp;
  if (x > 3 || x < 0) {
    throw Caml_builtin_exceptions.not_found;
  } else {
    switch (x) {
      case 0 : 
          tmp = (function (x) {
              return x + 1 | 0;
            });
          break;
      case 1 : 
          tmp = (function (x) {
              return x + 2 | 0;
            });
          break;
      case 2 : 
          tmp = (function (x) {
              return x + 3 | 0;
            });
          break;
      case 3 : 
          tmp = (function (x) {
              return x + 4 | 0;
            });
          break;
      
    }
  }
  return tmp(3);
}

exports.f0 = f0;
exports.f1 = f1;
exports.f3 = f3;
/* No side effect */
