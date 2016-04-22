// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");

function idiv(x, y) {
  if (y === 0) {
    throw Caml_builtin_exceptions.division_by_zero;
  }
  else {
    return x / y | 0;
  }
}

function imod(x, y) {
  if (y === 0) {
    throw Caml_builtin_exceptions.division_by_zero;
  }
  else {
    return x % y;
  }
}

exports.idiv = idiv;
exports.imod = imod;
/* No side effect */
