'use strict';

var Caml_array = require("../../lib/js/caml_array.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var x = [
  1,
  2
];

var y;

try {
  y = Caml_array.caml_array_get(x, 3);
}
catch (raw_msg){
  var msg = Caml_js_exceptions.internalToOCamlException(raw_msg);
  if (msg.CamlExt.CamlId === Caml_builtin_exceptions.invalid_argument.CamlId) {
    console.log(msg._1);
    y = 0;
  } else {
    throw msg;
  }
}

exports.x = x;
exports.y = y;
/* y Not a pure module */
