// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");

function caml_array_sub(x, offset, len) {
  var result = new Array(len);
  for(var j = 0 ,j_finish = len - 1; j<= j_finish; ++j){
    result[j] = x[offset + j | 0];
  }
  return result;
}

function caml_array_set(xs, index, newval) {
  if (index < 0 || index >= xs.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "index out of bounds"
        ];
  }
  else {
    xs[index] = newval;
    return /* () */0;
  }
}

function caml_array_get(xs, index) {
  if (index < 0 || index >= xs.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "index out of bounds"
        ];
  }
  else {
    return xs[index];
  }
}

function caml_make_vect(len, init) {
  var b = new Array(len);
  for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
    b[i] = init;
  }
  return b;
}

exports.caml_array_sub = caml_array_sub;
exports.caml_array_set = caml_array_set;
exports.caml_array_get = caml_array_get;
exports.caml_make_vect = caml_make_vect;
/* No side effect */
