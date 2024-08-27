'use strict';

let Caml = require("./caml.js");

function succ(n) {
  return n + 1 | 0;
}

function pred(n) {
  return n - 1 | 0;
}

function abs(n) {
  if (n >= 0) {
    return n;
  } else {
    return -n | 0;
  }
}

function lognot(n) {
  return n ^ -1;
}

let compare = Caml.int_compare;

function equal(x, y) {
  return x === y;
}

let zero = 0;

let one = 1;

let minus_one = -1;

let max_int = 2147483647;

let min_int = -2147483648;

exports.zero = zero;
exports.one = one;
exports.minus_one = minus_one;
exports.succ = succ;
exports.pred = pred;
exports.abs = abs;
exports.max_int = max_int;
exports.min_int = min_int;
exports.lognot = lognot;
exports.compare = compare;
exports.equal = equal;
/* No side effect */
