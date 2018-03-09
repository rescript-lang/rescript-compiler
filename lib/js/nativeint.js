'use strict';

var Caml_format = require("./caml_format.js");
var Caml_primitive = require("./caml_primitive.js");

function succ(n) {
  return n + 1;
}

function pred(n) {
  return n - 1;
}

function abs(n) {
  if (n >= 0) {
    return n;
  } else {
    return -n;
  }
}

function lognot(n) {
  return n ^ -1;
}

function to_string(n) {
  return Caml_format.caml_nativeint_format("%d", n);
}

var compare = Caml_primitive.caml_nativeint_compare;

var zero = 0;

var one = 1;

var minus_one = -1;

var size = 54;

var max_int = 9007199254740991;

var min_int = -9007199254740991;

exports.zero = zero;
exports.one = one;
exports.minus_one = minus_one;
exports.succ = succ;
exports.pred = pred;
exports.abs = abs;
exports.size = size;
exports.max_int = max_int;
exports.min_int = min_int;
exports.lognot = lognot;
exports.to_string = to_string;
exports.compare = compare;
/* No side effect */
