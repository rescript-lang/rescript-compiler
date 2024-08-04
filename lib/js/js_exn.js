'use strict';


function raiseThrow(exn) {
  throw exn
}
;

function raiseEvalError(str) {
  return raiseThrow(new EvalError(str));
}

function raiseRangeError(str) {
  return raiseThrow(new RangeError(str));
}

function raiseReferenceError(str) {
  return raiseThrow(new ReferenceError(str));
}

function raiseSyntaxError(str) {
  return raiseThrow(new SyntaxError(str));
}

function raiseTypeError(str) {
  return raiseThrow(new TypeError(str));
}

function raiseUriError(str) {
  return raiseThrow(new URIError(str));
}

function raiseError(str) {
  throw new Error("Failure", {
    cause: {
      RE_EXN_ID: "Failure",
      _1: str
    }
  });
}

let $$Error = "JsError";

exports.$$Error = $$Error;
exports.raiseError = raiseError;
exports.raiseEvalError = raiseEvalError;
exports.raiseRangeError = raiseRangeError;
exports.raiseReferenceError = raiseReferenceError;
exports.raiseSyntaxError = raiseSyntaxError;
exports.raiseTypeError = raiseTypeError;
exports.raiseUriError = raiseUriError;
/*  Not a pure module */
