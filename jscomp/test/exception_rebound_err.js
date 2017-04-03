'use strict';

var Curry                   = require("../../lib/js/curry.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Caml_exceptions         = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var A = Caml_exceptions.create("Exception_rebound_err.A");

var B = Caml_exceptions.create("Exception_rebound_err.B");

var C = Caml_exceptions.create("Exception_rebound_err.C");

function test_js_error4() {
  try {
    JSON.parse(" {\"x\"}");
    return 1;
  }
  catch (raw_e){
    var e = Js_exn.internalToOCamlException(raw_e);
    var exit = 0;
    if (e === Caml_builtin_exceptions.not_found) {
      return 2;
    } else if (e[0] === Caml_builtin_exceptions.invalid_argument) {
      if (e[1] === "x") {
        return 3;
      } else {
        exit = 1;
      }
    } else {
      exit = 1;
    }
    if (exit === 1) {
      if (e[0] === A) {
        if (e[1] !== 2) {
          if (e === B) {
            return 5;
          } else if (e[0] === C && !(e[1] !== 1 || e[2] !== 2)) {
            return 6;
          } else {
            return 7;
          }
        } else {
          return 4;
        }
      } else if (e === B) {
        return 5;
      } else if (e[0] === C && !(e[1] !== 1 || e[2] !== 2)) {
        return 6;
      } else {
        return 7;
      }
    }
    
  }
}

function f(g) {
  try {
    return Curry._1(g, /* () */0);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return 1;
    } else {
      throw exn;
    }
  }
}

exports.A              = A;
exports.B              = B;
exports.C              = C;
exports.test_js_error4 = test_js_error4;
exports.f              = f;
/* No side effect */
