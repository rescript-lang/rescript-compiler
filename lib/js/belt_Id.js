'use strict';

var Curry = require("./curry.js");

function MakeComparableU(M) {
  var cmp = M[/* cmp */0];
  return /* module */[/* cmp */cmp];
}

function MakeComparable(M) {
  var cmp = function (a, b) {
    return Curry._2(M[/* cmp */0], a, b);
  };
  return /* module */[/* cmp */cmp];
}

function comparableU(cmp) {
  return /* module */[/* cmp */cmp];
}

function comparable(cmp) {
  var M = /* module */[/* cmp */Curry.__2(cmp)];
  var cmp$1 = M[/* cmp */0];
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
  var hash = function (a) {
    return Curry._1(M[/* hash */0], a);
  };
  var eq = function (a, b) {
    return Curry._2(M[/* eq */1], a, b);
  };
  return /* module */[
          /* hash */hash,
          /* eq */eq
        ];
}

function hashableU(hash, eq) {
  return /* module */[
          /* hash */hash,
          /* eq */eq
        ];
}

function hashable(hash, eq) {
  var M_000 = Curry.__1(hash);
  var M_001 = Curry.__2(eq);
  var hash$1 = M_000;
  var eq$1 = M_001;
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
