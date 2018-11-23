'use strict';

var Caml_exceptions = require("./caml_exceptions.js");

var $$Error = Caml_exceptions.create("Caml_js_exceptions.Error");

function internalToOCamlException(e) {
  if (Caml_exceptions.isCamlExceptionOrOpenVariant(e)) {
    return e;
  } else {
    return [
            $$Error,
            e
          ];
  }
}

exports.$$Error = $$Error;
exports.internalToOCamlException = internalToOCamlException;
/* No side effect */
