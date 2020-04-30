'use strict';

var Test_common = require("./test_common.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Local = Caml_exceptions.create("Test_exception.Local");

function f(param) {
  throw {
        ExceptionID: Local.ExceptionID,
        _1: 3,
        Debug: Local.Debug
      };
}

function g(param) {
  throw {
        ExceptionID: -6,
        Debug: "Not_found"
      };
}

function h(param) {
  throw {
        ExceptionID: Test_common.U.ExceptionID,
        _1: 3,
        Debug: Test_common.U.Debug
      };
}

function x(param) {
  throw {
        ExceptionID: Test_common.H.ExceptionID,
        Debug: Test_common.H.Debug
      };
}

function xx(param) {
  throw {
        ExceptionID: -3,
        _1: "x",
        Debug: "Invalid_argument"
      };
}

var Nullary = Caml_exceptions.create("Test_exception.Nullary");

var a_000 = Nullary.ExceptionID;

var a_001 = Nullary.Debug;

var a = {
  ExceptionID: a_000,
  Debug: a_001
};

exports.Local = Local;
exports.f = f;
exports.g = g;
exports.h = h;
exports.x = x;
exports.xx = xx;
exports.Nullary = Nullary;
exports.a = a;
/* No side effect */
