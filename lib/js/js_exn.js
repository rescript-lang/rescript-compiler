'use strict';

var Caml_exceptions = require("./caml_exceptions.js");
var Caml_exceptions = require("./caml_exceptions.js");

var $$Error = Caml_exceptions.create("Js_exn.Error");

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

function error(str) {
  throw new Error(str);
}

function evalError(str) {
  throw new EvalError(str);
}

function rangeError(str) {
  throw new RangeError(str);
}

function referenceError(str) {
  throw new RerferenceError(str);
}

function syntaxError(str) {
  throw new SyntaxError(str);
}

function typeError(str) {
  throw new TypeError(str);
}

function uriError(str) {
  throw new URIError(str);
}

exports.$$Error                  = $$Error;
exports.internalToOCamlException = internalToOCamlException;
exports.error                    = error;
exports.evalError                = evalError;
exports.rangeError               = rangeError;
exports.referenceError           = referenceError;
exports.syntaxError              = syntaxError;
exports.typeError                = typeError;
exports.uriError                 = uriError;
/* No side effect */
