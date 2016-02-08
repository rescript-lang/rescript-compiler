// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Bytes       = require("../stdlib/bytes");
var Caml_curry  = require("../runtime/caml_curry");
var Caml_string = require("../runtime/caml_string");

function f(v) {
  return Caml_curry.app1(Bytes.unsafe_to_string, v);
}

function ff(v) {
  return Caml_string.bytes_to_string(v);
}

exports.f  = f;
exports.ff = ff;
/* No side effect */
