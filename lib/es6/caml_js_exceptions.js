

import * as Caml_option from "./caml_option.js";
import * as Caml_exceptions from "./caml_exceptions.js";

var $$Error = "JsError";

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

function as_js_exn(exn) {
  if (exn.RE_EXN_ID === $$Error) {
    return Caml_option.some(exn._1);
  }
  
}

export {
  $$Error ,
  internalToOCamlException ,
  as_js_exn ,
}
/* Caml_exceptions Not a pure module */
