

import * as Caml_js_exceptions from "./caml_js_exceptions.js";

function raiseWhenNotFound(x) {
  if (x == null) {
    throw Caml_js_exceptions.internalMakeExn("Not_found");
  }
  return x;
}

export {
  raiseWhenNotFound,
}
/* No side effect */
