'use strict';

var Caml_array = require("../../lib/js/caml_array.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var x = /* array */[
  1,
  2
];

var y;

try {
  y = Caml_array.caml_array_get(x, 3);
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
    console.log(exn[1]);
    y = 0;
  } else {
    throw exn;
  }
}

exports.x = x;
exports.y = y;
/* y Not a pure module */
