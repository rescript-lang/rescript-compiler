'use strict';

let Caml = require("./caml.js");
let Caml_format = require("./caml_format.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

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

function to_string(n) {
  return Caml_format.format_int("%d", n);
}

function of_string_opt(s) {
  try {
    return Caml_format.int_of_string(s);
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return;
    }
    throw exn;
  }
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
exports.of_string_opt = of_string_opt;
exports.to_string = to_string;
exports.compare = compare;
exports.equal = equal;
/* No side effect */
