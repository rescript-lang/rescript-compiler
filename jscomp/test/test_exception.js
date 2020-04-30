'use strict';

var Test_common = require("./test_common.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Local = Caml_exceptions.create("Test_exception.Local");

function f(param) {
  throw {
        ExceptionID: Local,
        _1: 3
      };
}

function g(param) {
  throw {
        ExceptionID: "Not_found"
      };
}

function h(param) {
  throw {
        ExceptionID: Test_common.U,
        _1: 3
      };
}

function x(param) {
  throw {
        ExceptionID: Test_common.H
      };
}

function xx(param) {
  throw {
        ExceptionID: "Invalid_argument",
        _1: "x"
      };
}

var Nullary = Caml_exceptions.create("Test_exception.Nullary");

var a = {
  ExceptionID: Nullary
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
