'use strict';

let Caml = require("./caml.js");

let compare = Caml.string_compare;

function equal(a, b) {
  return a === b;
}

exports.compare = compare;
exports.equal = equal;
/* No side effect */
