'use strict';

var Curry = require("./curry.js");

function comparableU(cmp) {
  return /* module */[/* cmp */cmp];
}

function comparable(cmp) {
  return comparableU(Curry.__2(cmp));
}

function hashableU(hash, eq) {
  return /* module */[
          /* hash */hash,
          /* eq */eq
        ];
}

function hashable(hash, eq) {
  return hashableU(Curry.__1(hash), Curry.__2(eq));
}

exports.comparableU = comparableU;
exports.comparable = comparable;
exports.hashableU = hashableU;
exports.hashable = hashable;
/* No side effect */
