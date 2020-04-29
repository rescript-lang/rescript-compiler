'use strict';

var Caml_array = require("../../lib/js/caml_array.js");

function caml_array_sub(x, offset, len) {
  var result = new Array(len);
  for(var j = 0; j < len; ++j){
    Caml_array.caml_array_set(result, j, Caml_array.caml_array_get(x, offset + j | 0));
  }
  return result;
}

function caml_array_set(xs, index, newval) {
  if (index < 0 || index >= xs.length) {
    throw {
          ExceptionID: -3,
          _1: "index out of bounds",
          Debug: "Invalid_argument"
        };
  }
  return Caml_array.caml_array_set(xs, index, newval);
}

function caml_array_get(xs, index) {
  if (index < 0 || index >= xs.length) {
    throw {
          ExceptionID: -3,
          _1: "index out of bounds",
          Debug: "Invalid_argument"
        };
  }
  return Caml_array.caml_array_get(xs, index);
}

function caml_make_vect(len, init) {
  var b = new Array(len);
  for(var i = 0; i < len; ++i){
    Caml_array.caml_array_set(b, i, init);
  }
  return b;
}

exports.caml_array_sub = caml_array_sub;
exports.caml_array_set = caml_array_set;
exports.caml_array_get = caml_array_get;
exports.caml_make_vect = caml_make_vect;
/* No side effect */
