'use strict';

let Caml_js_exceptions = require("./caml_js_exceptions.js");

function div(x, y) {
  if (y === 0) {
    throw Caml_js_exceptions.internalMakeExn("Division_by_zero");
  }
  return x / y | 0;
}

function mod_(x, y) {
  if (y === 0) {
    throw Caml_js_exceptions.internalMakeExn("Division_by_zero");
  }
  return x % y;
}

exports.div = div;
exports.mod_ = mod_;
/* No side effect */
