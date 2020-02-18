

import * as Caml_option from "./caml_option.js";
import * as Caml_exceptions from "./caml_exceptions.js";

var $$Error = Caml_exceptions.create("Caml_js_exceptions.Error");

function internalToOCamlException(e) {
  if (Caml_exceptions.caml_is_extension(e)) {
    return e;
  } else {
    return [
            $$Error,
            e
          ];
  }
}

function caml_as_js_exn(exn) {
  if (exn[0] === $$Error) {
    return Caml_option.some(exn[1]);
  }
  
}

export {
  $$Error ,
  internalToOCamlException ,
  caml_as_js_exn ,
  
}
/* No side effect */
