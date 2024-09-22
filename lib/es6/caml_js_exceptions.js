

import * as Caml_exceptions from "./caml_exceptions.js";

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

export {
  Obj,
  $$Error,
  internalToOCamlException,
}
/* No side effect */
