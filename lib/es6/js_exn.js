


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

var $$Error$1 = "JsError";

export {
  $$Error$1 as $$Error,
  raiseError ,
  raiseEvalError ,
  raiseRangeError ,
  raiseReferenceError ,
  raiseSyntaxError ,
  raiseTypeError ,
  raiseUriError ,
}
/* No side effect */
