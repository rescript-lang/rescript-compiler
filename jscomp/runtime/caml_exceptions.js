// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");

function caml_raise_sys_error(msg) {
  throw [
        Caml_builtin_exceptions.sys_error,
        msg
      ];
}

function caml_failwith(s) {
  throw [
        Caml_builtin_exceptions.failure,
        s
      ];
}

function caml_invalid_argument(s) {
  throw [
        Caml_builtin_exceptions.invalid_argument,
        s
      ];
}

function caml_array_bound_error() {
  throw [
        Caml_builtin_exceptions.invalid_argument,
        "index out of bounds"
      ];
}

function caml_raise_zero_divide() {
  throw Caml_builtin_exceptions.division_by_zero;
}

function caml_raise_not_found() {
  throw Caml_builtin_exceptions.not_found;
}

function caml_undef_module(loc) {
  throw [
        Caml_builtin_exceptions.undefined_recursive_module,
        loc
      ];
}

exports.caml_raise_sys_error   = caml_raise_sys_error;
exports.caml_failwith          = caml_failwith;
exports.caml_invalid_argument  = caml_invalid_argument;
exports.caml_array_bound_error = caml_array_bound_error;
exports.caml_raise_zero_divide = caml_raise_zero_divide;
exports.caml_raise_not_found   = caml_raise_not_found;
exports.caml_undef_module      = caml_undef_module;
/* No side effect */
