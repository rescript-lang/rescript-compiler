'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function get(s, i) {
  if (i >= s.length || i < 0) {
    throw {
          CamlExt: Caml_builtin_exceptions.invalid_argument,
          _1: "index out of bounds"
        };
  }
  return s.charCodeAt(i);
}

exports.get = get;
/* No side effect */
