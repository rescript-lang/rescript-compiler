'use strict';

let Primitive_hash = require("./primitive_hash.js");

function hash(x) {
  return Primitive_hash.hash(10, 100, 0, x);
}

exports.hash = hash;
/* No side effect */
