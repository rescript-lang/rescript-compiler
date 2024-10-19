'use strict';


let $$EvalError = {};

let $$RangeError = {};

let $$ReferenceError = {};

let $$SyntaxError = {};

let $$TypeError = {};

let $$URIError = {};

function panic(msg) {
  throw new Error("Panic! " + msg);
}

exports.$$EvalError = $$EvalError;
exports.$$RangeError = $$RangeError;
exports.$$ReferenceError = $$ReferenceError;
exports.$$SyntaxError = $$SyntaxError;
exports.$$TypeError = $$TypeError;
exports.$$URIError = $$URIError;
exports.panic = panic;
/* No side effect */
