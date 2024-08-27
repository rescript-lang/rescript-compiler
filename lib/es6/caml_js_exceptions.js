

import * as Caml_option from "./caml_option.js";

let $$Error = "JsError";

function internalAnyToExn(any) {
  if (any && typeof any.RE_EXN_ID === "string") {
    return any;
  } else {
    return {
      RE_EXN_ID: "JsError",
      _1: any
    };
  }
}

class RescriptError extends Error {
  constructor(message) {
    super(message);
    this.RE_EXN_ID = message;
  }
}
;

function internalMakeExn(prim) {
  return new RescriptError(prim);
}

function internalFromExtension(_ext) {
  return (Object.assign(new RescriptError(_ext.RE_EXN_ID), _ext));
}

function as_js_exn(exn) {
  if (exn.RE_EXN_ID === $$Error) {
    return Caml_option.some(exn._1);
  }
  
}

export {
  $$Error,
  internalAnyToExn,
  internalMakeExn,
  internalFromExtension,
  as_js_exn,
}
/*  Not a pure module */
