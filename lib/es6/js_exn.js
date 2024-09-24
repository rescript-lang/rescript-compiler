

import * as Primitive_option from "./primitive_option.js";

let $$Error = "JsError";

function asJsExn(exn) {
  let t = exn;
  if (t.RE_EXN_ID === $$Error) {
    return Primitive_option.some(t._1);
  }
  
}

function raiseError(str) {
  throw new Error(str);
}

function raiseEvalError(str) {
  throw new EvalError(str);
}

function raiseRangeError(str) {
  throw new RangeError(str);
}

function raiseReferenceError(str) {
  throw new ReferenceError(str);
}

function raiseSyntaxError(str) {
  throw new SyntaxError(str);
}

function raiseTypeError(str) {
  throw new TypeError(str);
}

function raiseUriError(str) {
  throw new URIError(str);
}

export {
  $$Error,
  asJsExn,
  raiseError,
  raiseEvalError,
  raiseRangeError,
  raiseReferenceError,
  raiseSyntaxError,
  raiseTypeError,
  raiseUriError,
}
/* No side effect */
