'use strict';

var Caml_string = require("../../lib/js/caml_string.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var hh;

try {
  hh = Caml_string.get("ghsogh", -3);
}
catch (raw_e){
  var e = Caml_js_exceptions.internalToOCamlException(raw_e);
  if (e.ExceptionID === "Invalid_argument") {
    console.log(e._1);
    hh = /* "a" */97;
  } else {
    throw e;
  }
}

var f = /* "o" */111;

exports.f = f;
exports.hh = hh;
/* hh Not a pure module */
