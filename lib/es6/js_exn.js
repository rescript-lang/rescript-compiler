


function raiseError(str) {
  throw new Error(new Error(str).RE_EXN_ID, {
        cause: new Error(str)
      });
}

function raiseEvalError(str) {
  throw new Error(new EvalError(str).RE_EXN_ID, {
        cause: new EvalError(str)
      });
}

function raiseRangeError(str) {
  throw new Error(new RangeError(str).RE_EXN_ID, {
        cause: new RangeError(str)
      });
}

function raiseReferenceError(str) {
  throw new Error(new ReferenceError(str).RE_EXN_ID, {
        cause: new ReferenceError(str)
      });
}

function raiseSyntaxError(str) {
  throw new Error(new SyntaxError(str).RE_EXN_ID, {
        cause: new SyntaxError(str)
      });
}

function raiseTypeError(str) {
  throw new Error(new TypeError(str).RE_EXN_ID, {
        cause: new TypeError(str)
      });
}

function raiseUriError(str) {
  throw new Error(new URIError(str).RE_EXN_ID, {
        cause: new URIError(str)
      });
}

let $$Error$1 = "JsError";

export {
  $$Error$1 as $$Error,
  raiseError,
  raiseEvalError,
  raiseRangeError,
  raiseReferenceError,
  raiseSyntaxError,
  raiseTypeError,
  raiseUriError,
}
/* No side effect */
