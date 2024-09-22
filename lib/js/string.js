'use strict';

let Primitive_string = require("./primitive_string.js");

function length(prim) {
  return prim.length;
}

function get(prim0, prim1) {
  return prim0.codePointAt(prim1);
}

function unsafe_get(prim0, prim1) {
  return prim0.codePointAt(prim1);
}

let compare = Primitive_string.compare;

function equal(a, b) {
  return a === b;
}

exports.length = length;
exports.get = get;
exports.unsafe_get = unsafe_get;
exports.compare = compare;
exports.equal = equal;
/* No side effect */
