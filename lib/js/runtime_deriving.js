'use strict';

let Caml_js_exceptions = require("./caml_js_exceptions.js");

function raiseWhenNotFound(x) {
  if (x == null) {
    throw Caml_js_exceptions.internalMakeExn("Not_found");
  }
  return x;
}

exports.raiseWhenNotFound = raiseWhenNotFound;
/* No side effect */
