'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");

function exnGet(dict, key) {
  var match = dict[key];
  if (match !== undefined) {
    return match;
  }
  else {
    var s = "Js_dict.exnGet: missing key " + key;
    throw [
          Caml_builtin_exceptions.invalid_argument,
          s
        ];
  }
}

exports.exnGet = exnGet;
/* No side effect */
