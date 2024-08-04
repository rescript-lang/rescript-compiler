'use strict';


let $$throw = (function (exn) {
  throw exn;
});

function raiseError(str) {
  return $$throw(new Error(str));
}

function raiseEvalError(str) {
  return $$throw(new EvalError(str));
}

function raiseRangeError(str) {
  return $$throw(new RangeError(str));
}

function raiseReferenceError(str) {
  return $$throw(new ReferenceError(str));
}

function raiseSyntaxError(str) {
  return $$throw(new SyntaxError(str));
}

function raiseTypeError(str) {
  return $$throw(new TypeError(str));
}

function raiseUriError(str) {
  return $$throw(new URIError(str));
}

let $$Error$1 = "JsError";

exports.$$Error = $$Error$1;
exports.raiseError = raiseError;
exports.raiseEvalError = raiseEvalError;
exports.raiseRangeError = raiseRangeError;
exports.raiseReferenceError = raiseReferenceError;
exports.raiseSyntaxError = raiseSyntaxError;
exports.raiseTypeError = raiseTypeError;
exports.raiseUriError = raiseUriError;
/* No side effect */
