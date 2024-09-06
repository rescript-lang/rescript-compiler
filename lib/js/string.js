'use strict';

let Primitive_string = require("./primitive_string.js");

let compare = Primitive_string.compare;

function equal(a, b) {
  return a === b;
}

exports.compare = compare;
exports.equal = equal;
/* No side effect */
