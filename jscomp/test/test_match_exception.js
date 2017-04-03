'use strict';

var Curry                   = require("../../lib/js/curry.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(g, x) {
  try {
    return Curry._1(g, x);
  }
  catch (exn){
    var exn$1 = Js_exn.internalToOCamlException(exn);
    if (exn$1 === Caml_builtin_exceptions.not_found) {
      return 3;
    } else {
      throw exn$1;
    }
  }
}

exports.f = f;
/* No side effect */
