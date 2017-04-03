'use strict';

var Curry                   = require("../../lib/js/curry.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Js_primitive            = require("../../lib/js/js_primitive.js");
var Caml_exceptions         = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f() {
  throw Caml_builtin_exceptions.not_found;
}

function assert_f(x) {
  if (x <= 3) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "exception_value_test.ml",
            9,
            12
          ]
        ];
  }
  return 3;
}

function hh() {
  throw Caml_builtin_exceptions.not_found;
}

var A = Caml_exceptions.create("Exception_value_test.A");

var u = [
  A,
  3
];

function test_not_found(f, _) {
  try {
    return Curry._1(f, /* () */0);
  }
  catch (exn){
    var exn$1 = Js_exn.internalToOCamlException(exn);
    if (exn$1 === Caml_builtin_exceptions.not_found) {
      return 2;
    } else {
      throw exn$1;
    }
  }
}

function test_js_error() {
  try {
    return JSON.parse(" {\"x\" : }");
  }
  catch (exn){
    var exn$1 = Js_exn.internalToOCamlException(exn);
    if (exn$1[0] === Js_exn.$$Error) {
      console.log(Js_primitive.undefined_to_opt(exn$1[1].stack));
      throw Caml_builtin_exceptions.not_found;
    } else {
      throw exn$1;
    }
  }
}

function test_js_error2() {
  try {
    return JSON.parse(" {\"x\" : }");
  }
  catch (e){
    var e$1 = Js_exn.internalToOCamlException(e);
    if (e$1[0] === Js_exn.$$Error) {
      console.log(Js_primitive.undefined_to_opt(e$1[1].stack));
      throw e$1;
    } else {
      throw e$1;
    }
  }
}

exports.f              = f;
exports.assert_f       = assert_f;
exports.hh             = hh;
exports.A              = A;
exports.u              = u;
exports.test_not_found = test_not_found;
exports.test_js_error  = test_js_error;
exports.test_js_error2 = test_js_error2;
/* No side effect */
