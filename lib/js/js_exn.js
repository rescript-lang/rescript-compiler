'use strict';

var Caml_exceptions = require("./caml_exceptions.js");
var Caml_exceptions = require("./caml_exceptions.js");

var $$Error = Caml_exceptions.create("Js_exn.Error");

function internalToOCamlException(e) {
  if (Caml_exceptions.isCamlException(e)) {
    return e;
  } else {
    return [
            $$Error,
            e
          ];
  }
}

exports.$$Error                  = $$Error;
exports.internalToOCamlException = internalToOCamlException;
/* No side effect */
