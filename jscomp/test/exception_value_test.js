'use strict';

var Curry = require("../../lib/js/curry.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function f(param) {
  throw {
        ExceptionID: -6,
        Debug: "Not_found"
      };
}

function assert_f(x) {
  if (x <= 3) {
    throw {
          ExceptionID: -9,
          _1: /* tuple */[
            "exception_value_test.ml",
            9,
            12
          ],
          Debug: "Assert_failure"
        };
  }
  return 3;
}

function hh(param) {
  throw {
        ExceptionID: -6,
        Debug: "Not_found"
      };
}

var A = Caml_exceptions.create("Exception_value_test.A");

var B = Caml_exceptions.create("Exception_value_test.B");

var C = Caml_exceptions.create("Exception_value_test.C");

var u_000 = A.ExceptionID;

var u_002 = A.Debug;

var u = {
  ExceptionID: u_000,
  _1: 3,
  Debug: u_002
};

function test_not_found(f, param) {
  try {
    return Curry._1(f, undefined);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === /* Not_found */-6) {
      return 2;
    }
    throw exn;
  }
}

function test_js_error2(param) {
  try {
    return JSON.parse(" {\"x\" : }");
  }
  catch (raw_e){
    var e = Caml_js_exceptions.internalToOCamlException(raw_e);
    if (e.ExceptionID === Js_exn.$$Error.ExceptionID) {
      console.log(e._1.stack);
      throw e;
    }
    throw e;
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
