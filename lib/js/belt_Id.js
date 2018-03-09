'use strict';

var Curry = require("./curry.js");

function MakeComparableU(M) {
  var cmp = M[/* cmp */0];
  return /* module */[/* cmp */cmp];
}

function MakeComparable(M) {
  var cmp = M[/* cmp */0];
  var cmp$1 = Curry.__2(cmp);
  return /* module */[/* cmp */cmp$1];
}

function comparableU(cmp) {
  return /* module */[/* cmp */cmp];
}

function comparable(cmp) {
  var cmp$1 = Curry.__2(cmp);
  return /* module */[/* cmp */cmp$1];
}

function MakeHashableU(M) {
  var hash = M[/* hash */0];
  var eq = M[/* eq */1];
  return /* module */[
          /* hash */hash,
          /* eq */eq
        ];
}

function MakeHashable(M) {
  var hash = M[/* hash */0];
  var hash$1 = Curry.__1(hash);
  var eq = M[/* eq */1];
  var eq$1 = Curry.__2(eq);
  return /* module */[
          /* hash */hash$1,
          /* eq */eq$1
        ];
}

function hashableU(hash, eq) {
  return /* module */[
          /* hash */hash,
          /* eq */eq
        ];
}

function hashable(hash, eq) {
  var hash$1 = Curry.__1(hash);
  var eq$1 = Curry.__2(eq);
  return /* module */[
          /* hash */hash$1,
          /* eq */eq$1
        ];
}

exports.MakeComparableU = MakeComparableU;
exports.MakeComparable = MakeComparable;
exports.comparableU = comparableU;
exports.comparable = comparable;
exports.MakeHashableU = MakeHashableU;
exports.MakeHashable = MakeHashable;
exports.hashableU = hashableU;
exports.hashable = hashable;
/* No side effect */
