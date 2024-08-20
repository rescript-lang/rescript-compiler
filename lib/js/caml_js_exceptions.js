'use strict';

let Caml_option = require("./caml_option.js");
let Caml_exceptions = require("./caml_exceptions.js");

let $$Error = "JsError";

function internalToOCamlException(e) {
  if (Caml_exceptions.is_extension(e.cause)) {
    return e.cause;
  } else {
    return new Error("JsError", {
      cause: {
        RE_EXN_ID: "JsError",
        _1: e
      }
    });
  }
}

function as_js_exn(exn) {
  if (exn.RE_EXN_ID === $$Error) {
    return Caml_option.some(exn._1);
  }
  
}

exports.$$Error = $$Error;
exports.internalToOCamlException = internalToOCamlException;
exports.as_js_exn = as_js_exn;
/* Caml_exceptions Not a pure module */
