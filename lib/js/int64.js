'use strict';

var Caml = require("./caml.js");
var Caml_int64 = require("./caml_int64.js");
var Caml_format = require("./caml_format.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");

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
    return Caml_format.caml_int64_of_string(s);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return ;
    }
    throw exn;
  }
}

var compare = Caml_int64.compare;

function equal(x, y) {
  return Caml_int64.compare(x, y) === 0;
}

var zero = Caml_int64.zero;

var one = Caml_int64.one;

var minus_one = Caml_int64.neg_one;

var succ = Caml_int64.succ;

var max_int = Caml_int64.max_int;

var min_int = Caml_int64.min_int;

var to_string = Caml_int64.to_string;

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
