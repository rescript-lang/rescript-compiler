'use strict';

var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Caml_option = require("./caml_option.js");

function caml_weak_create(n) {
  return new Array(n);
}

function caml_weak_set(xs, i, v) {
  if (v !== undefined) {
    xs[i] = Caml_option.valFromOption(v);
    return /* () */0;
  } else {
    return /* () */0;
  }
}

function caml_weak_get(xs, i) {
  return Caml_option.undefined_to_opt(xs[i]);
}

function caml_weak_get_copy(xs, i) {
  var match = xs[i];
  if (match !== undefined) {
    return Caml_option.some(Caml_obj.caml_obj_dup(match));
  }
  
}

function caml_weak_check(xs, i) {
  return xs[i] !== undefined;
}

var caml_weak_blit = Caml_array.caml_array_blit;

exports.caml_weak_create = caml_weak_create;
exports.caml_weak_set = caml_weak_set;
exports.caml_weak_get = caml_weak_get;
exports.caml_weak_get_copy = caml_weak_get_copy;
exports.caml_weak_check = caml_weak_check;
exports.caml_weak_blit = caml_weak_blit;
/* No side effect */
