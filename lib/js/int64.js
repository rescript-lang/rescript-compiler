'use strict';

let Caml = require("./caml.js");
let Caml_int64 = require("./caml_int64.js");
let Caml_format = require("./caml_format.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

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

function of_string_opt(s) {
  try {
    return Caml_format.int64_of_string(s);
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return;
    }
    throw exn;
  }
}

let compare = Caml_int64.compare;

let equal = Caml_int64.equal;

let zero = Caml_int64.zero;

let one = Caml_int64.one;

let minus_one = Caml_int64.neg_one;

let succ = Caml_int64.succ;

let max_int = Caml_int64.max_int;

let min_int = Caml_int64.min_int;

let to_string = Caml_int64.to_string;

exports.zero = zero;
exports.one = one;
exports.minus_one = minus_one;
exports.succ = succ;
exports.pred = pred;
exports.abs = abs;
exports.max_int = max_int;
exports.min_int = min_int;
exports.lognot = lognot;
exports.of_string_opt = of_string_opt;
exports.to_string = to_string;
exports.compare = compare;
exports.equal = equal;
/* No side effect */
