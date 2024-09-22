'use strict';

let Caml_exceptions = require("./caml_exceptions.js");

function internalToOCamlException(e) {
  if (Caml_exceptions.is_extension(e)) {
    return e;
  } else {
    return {
      RE_EXN_ID: "JsError",
      _1: e
    };
  }
}

let Obj;

let $$Error = "JsError";

exports.Obj = Obj;
exports.$$Error = $$Error;
exports.internalToOCamlException = internalToOCamlException;
/* No side effect */
