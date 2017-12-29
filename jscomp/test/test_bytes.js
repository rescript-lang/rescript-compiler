'use strict';

var Caml_string = require("../../lib/js/caml_string.js");

var f = Caml_string.bytes_to_string;

var ff = Caml_string.bytes_to_string;

exports.f = f;
exports.ff = ff;
/* No side effect */
