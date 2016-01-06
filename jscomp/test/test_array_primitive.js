// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("../stdlib/pervasives");

function caml_array_sub(x, offset, len) {
  var result = new Array(len);
  for(var j = 0 ,j_finish = len - 1; j<= j_finish; ++j){
    result[j] = x[offset + j];
  }
  return result;
}

function caml_array_set(xs, index, newval) {
  return index < 0 || index >= xs.length ? Pervasives.invalid_arg("index out of bounds") : (xs[index] = newval, /* () */0);
}

function caml_array_get(xs, index) {
  return index < 0 || index >= xs.length ? Pervasives.invalid_arg("index out of bounds") : xs[index];
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
