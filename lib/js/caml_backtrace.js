'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function caml_convert_raw_backtrace_slot() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_convert_raw_backtrace_slot unimplemented"
      ];
}

exports.caml_convert_raw_backtrace_slot = caml_convert_raw_backtrace_slot;
/* No side effect */
