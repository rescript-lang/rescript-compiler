'use strict';

var Js_exn = require("../../lib/js/js_exn.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var hh;

try {
  hh = Caml_string.get("ghsogh", -3);
}
catch (raw_exn){
  var exn = Js_exn.internalToOCamlException(raw_exn);
  if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
    console.log(exn[1]);
    hh = /* "a" */97;
  } else {
    throw exn;
  }
}

var f = /* "o" */111;

exports.f = f;
exports.hh = hh;
/* hh Not a pure module */
