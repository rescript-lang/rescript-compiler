'use strict';

var Bytes = require("../../lib/js/bytes.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");

var f = Bytes.unsafe_to_string;

var ff = Caml_bytes.bytes_to_string;

exports.f = f;
exports.ff = ff;
/* No side effect */
