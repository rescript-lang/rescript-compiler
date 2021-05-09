'use strict';

var $$Set = require("../../lib/js/set.js");
var Caml = require("../../lib/js/caml.js");
var Curry = require("../../lib/js/curry.js");

function compare(t1, t2) {
  if (t1.TAG === /* Leaf */0) {
    if (t2.TAG === /* Leaf */0) {
      return Caml.string_compare(t1._0, t2._0);
    } else {
      return 1;
    }
  } else if (t2.TAG === /* Leaf */0) {
    return -1;
  } else {
    return Curry._2(ASet.compare, t1._0, t2._0);
  }
}

var A = {
  compare: compare
};

var ASet = $$Set.Make(A);

var X0 = {};

var Y0 = {};

function f(x) {
  return x + 1 | 0;
}

var X1 = {
  f: f
};

function f$1(x) {
  return x + 2 | 0;
}

var Y1 = {
  f: f$1
};

var X;

exports.A = A;
exports.ASet = ASet;
exports.X = X;
exports.X0 = X0;
exports.Y0 = Y0;
exports.X1 = X1;
exports.Y1 = Y1;
/* ASet Not a pure module */
