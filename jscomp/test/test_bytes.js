'use strict';

var Caml_bytes = require("../../lib/js/caml_bytes.js");

var f = Caml_bytes.bytes_to_string;

var ff = Caml_bytes.bytes_to_string;

exports.f = f;
exports.ff = ff;
/* No side effect */
