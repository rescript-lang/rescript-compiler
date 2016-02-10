// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");

function caml_raise_sys_error(msg) {
  throw [
        0,
        Caml_builtin_exceptions.Sys_error,
        msg
      ];
}

function caml_failwith(s) {
  throw [
        0,
        Caml_builtin_exceptions.Failure,
        s
      ];
}

function caml_invalid_argument(s) {
  throw [
        0,
        Caml_builtin_exceptions.Invalid_argument,
        s
      ];
}

function caml_array_bound_error() {
  throw [
        0,
        Caml_builtin_exceptions.Invalid_argument,
        "index out of bounds"
      ];
}

function caml_raise_zero_divide() {
  throw Caml_builtin_exceptions.Division_by_zero;
}

function caml_raise_not_found() {
  throw Caml_builtin_exceptions.Not_found;
}

function caml_undef_module(loc) {
  throw [
        0,
        Caml_builtin_exceptions.Undefined_recursive_module,
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
