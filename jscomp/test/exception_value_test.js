'use strict';

var Curry = require("../../lib/js/curry.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(param) {
  throw Caml_builtin_exceptions.not_found;
}

function assert_f(x) {
  if (x <= 3) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "exception_value_test.ml",
            9,
            12
          ]
        ];
  }
  return 3;
}

function hh(param) {
  throw Caml_builtin_exceptions.not_found;
}

var A = Caml_exceptions.create("Exception_value_test.A");

var B = Caml_exceptions.create("Exception_value_test.B");

var C = Caml_exceptions.create("Exception_value_test.C");

var u = [
  A,
  3
];

function test_not_found(f, param) {
  try {
    return Curry._1(f, /* () */0);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return 2;
    } else {
      throw exn;
    }
  }
}

function test_js_error2(param) {
  try {
    return JSON.parse(" {\"x\" : }");
  }
  catch (raw_e){
    var e = Caml_js_exceptions.internalToOCamlException(raw_e);
    if (e[0] === Js_exn.$$Error) {
      console.log(e[1].stack);
      throw e;
    } else {
      throw e;
    }
  }
}

function test_js_error3(param) {
  try {
    JSON.parse(" {\"x\"}");
    return 1;
  }
  catch (e){
    return 0;
  }
}

exports.f = f;
exports.assert_f = assert_f;
exports.hh = hh;
exports.A = A;
exports.B = B;
exports.C = C;
exports.u = u;
exports.test_not_found = test_not_found;
exports.test_js_error2 = test_js_error2;
exports.test_js_error3 = test_js_error3;
/* No side effect */
