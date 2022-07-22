'use strict';

var Bytes = require("../../lib/js/bytes.js");

var f = Bytes.unsafe_to_string;

var ff = Bytes.bytes_to_string;

exports.f = f;
exports.ff = ff;
/* No side effect */
