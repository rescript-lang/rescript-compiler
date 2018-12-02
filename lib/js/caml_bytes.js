'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function get(s, i) {
  if (i < 0 || i >= s.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "index out of bounds"
        ];
  } else {
    return s[i];
  }
}

function caml_fill_bytes(s, i, l, c) {
  if (l > 0) {
    for(var k = i ,k_finish = (l + i | 0) - 1 | 0; k <= k_finish; ++k){
      s[k] = c;
    }
    return /* () */0;
  } else {
    return 0;
  }
}

function caml_create_bytes(len) {
  if (len < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "String.create"
        ];
  } else {
    var result = new Array(len);
    for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      result[i] = /* "\000" */0;
    }
    return result;
  }
}

exports.caml_create_bytes = caml_create_bytes;
exports.caml_fill_bytes = caml_fill_bytes;
exports.get = get;
/* No side effect */
