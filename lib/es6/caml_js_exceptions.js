

import * as Caml_option from "./caml_option.js";
import * as Caml_exceptions from "./caml_exceptions.js";

var $$Error = /* @__PURE__ */Caml_exceptions.create("Caml_js_exceptions.Error");

function internalToOCamlException(e) {
  if (Caml_exceptions.caml_is_extension(e)) {
    return e;
  } else {
    return {
            RE_EXN_ID: $$Error,
            _1: e
          };
  }
}

function caml_as_js_exn(exn) {
  if (exn.RE_EXN_ID === $$Error) {
    return Caml_option.some(exn._1);
  }
  
}

export {
  $$Error ,
  internalToOCamlException ,
  caml_as_js_exn ,
  
}
/* No side effect */
