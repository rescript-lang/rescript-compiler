'use strict';

let Caml_hash = require("./caml_hash.js");

function hash(x) {
  return Caml_hash.hash(10, 100, 0, x);
}

exports.hash = hash;
/* No side effect */
