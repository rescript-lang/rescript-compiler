'use strict';

let Caml = require("./caml.js");
let Caml_int64 = require("./caml_int64.js");

function pred(n) {
  return Caml_int64.sub(n, Caml_int64.one);
}

function abs(n) {
  if (Caml.i64_ge(n, Caml_int64.zero)) {
    return n;
  } else {
    return Caml_int64.neg(n);
  }
}

function lognot(n) {
  return Caml_int64.xor(n, Caml_int64.neg_one);
}

let compare = Caml_int64.compare;

let equal = Caml_int64.equal;

let zero = Caml_int64.zero;

let one = Caml_int64.one;

let minus_one = Caml_int64.neg_one;

let succ = Caml_int64.succ;

let max_int = Caml_int64.max_int;

let min_int = Caml_int64.min_int;

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
