'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function f(g, x) {
  try {
    return Curry._1(g, x);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === /* Not_found */-6) {
      return 3;
    }
    throw exn;
  }
}

exports.f = f;
/* No side effect */
