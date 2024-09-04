'use strict';

let Caml_js_exceptions = require("./caml_js_exceptions.js");

function get(s, i) {
  if (i >= s.length || i < 0) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "index out of bounds"
    });
  }
  return s.codePointAt(i);
}

function make(n, ch) {
  return String.fromCharCode(ch).repeat(n);
}

exports.get = get;
exports.make = make;
/* No side effect */
