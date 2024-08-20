

import * as Caml_option from "./caml_option.js";
import * as Caml_exceptions from "./caml_exceptions.js";

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

export {
  $$Error,
  internalToOCamlException,
  as_js_exn,
}
/* Caml_exceptions Not a pure module */
