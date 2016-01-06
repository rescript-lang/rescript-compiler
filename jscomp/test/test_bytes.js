// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Bytes = require("../stdlib/bytes");
var Caml_string = require("../runtime/caml_string");

function f(v) {
  return Bytes.unsafe_to_string(v);
}

function ff(v) {
  return Caml_string.bytes_to_string(v);
}

exports.f = f;
exports.ff = ff;
/* No side effect */
