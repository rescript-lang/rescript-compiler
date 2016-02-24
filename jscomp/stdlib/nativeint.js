// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj    = require("../runtime/caml_obj");
var Caml_format = require("../runtime/caml_format");
var Sys         = require("./sys");

function succ(n) {
  return n + 1 | 0;
}

function pred(n) {
  return n - 1 | 0;
}

function abs(n) {
  if (n >= 0) {
    return n;
  }
  else {
    return -n | 0;
  }
}

var size = Sys.word_size;

var min_int = -9007199254740991;

var max_int = 9007199254740991;

function lognot(n) {
  return n ^ -1;
}

function to_string(n) {
  return Caml_format.caml_nativeint_format("%d", n);
}

function compare(x, y) {
  return Caml_obj.caml_nativeint_compare(x, y);
}

var zero = 0;

var one = 1;

var minus_one = -1;

exports.zero      = zero;
exports.one       = one;
exports.minus_one = minus_one;
exports.succ      = succ;
exports.pred      = pred;
exports.abs       = abs;
exports.size      = size;
exports.max_int   = max_int;
exports.min_int   = min_int;
exports.lognot    = lognot;
exports.to_string = to_string;
exports.compare   = compare;
/* No side effect */
