'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(match) {
  if (Caml_exceptions.isCamlExceptionOrOpenVariant(match)) {
    if (match === Caml_builtin_exceptions.not_found) {
      return [0];
    } else if (match[0] === Caml_builtin_exceptions.invalid_argument || match === Caml_builtin_exceptions.stack_overflow) {
      return [1];
    } else if (match[0] === Caml_builtin_exceptions.sys_error) {
      return [2];
    } else {
      return /* None */0;
    }
  } else {
    return /* None */0;
  }
}

var A = Caml_exceptions.create("Exn_error_pattern.A");

var B = Caml_exceptions.create("Exn_error_pattern.B");

function g(match) {
  if (Caml_exceptions.isCamlExceptionOrOpenVariant(match)) {
    if (match === Caml_builtin_exceptions.not_found || match[0] === Caml_builtin_exceptions.invalid_argument) {
      return [0];
    } else if (match[0] === Caml_builtin_exceptions.sys_error) {
      return [2];
    } else if (match[0] === A || match[0] === B) {
      return [match[1]];
    } else {
      return /* None */0;
    }
  } else {
    return /* None */0;
  }
}

exports.f = f;
exports.A = A;
exports.B = B;
exports.g = g;
/* No side effect */
