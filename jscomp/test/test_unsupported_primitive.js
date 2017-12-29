'use strict';

var Caml_missing_polyfill = require("../../lib/js/caml_missing_polyfill.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function to_buffer(buff, ofs, len, _, _$1) {
  if (ofs < 0 || len < 0 || ofs > (buff.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Marshal.to_buffer: substring out of bounds"
        ];
  } else {
    return Caml_missing_polyfill.not_implemented("caml_output_value_to_buffer not implemented by bucklescript yet\n");
  }
}

exports.to_buffer = to_buffer;
/* No side effect */
