'use strict';

let Primitive_hash = require("./Primitive_hash.js");

function hash(x) {
  return Primitive_hash.hash(10, 100, 0, x);
}

exports.hash = hash;
/* No side effect */
