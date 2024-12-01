// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

function f(g, x) {
  try {
    return g(x);
  } catch (raw_exn) {
    let exn = Primitive_exceptions.internalToException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return 3;
    }
    throw exn;
  }
}

export {
  f,
}
/* No side effect */
