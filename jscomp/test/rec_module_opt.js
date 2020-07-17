'use strict';

var $$Set = require("../../lib/js/set.js");
var Curry = require("../../lib/js/curry.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

function compare(t1, t2) {
  if (t1.TAG) {
    if (t2.TAG) {
      return Curry._2(ASet.compare, t1._0, t2._0);
    } else {
      return -1;
    }
  } else if (t2.TAG) {
    return 1;
  } else {
    return Caml_primitive.caml_string_compare(t1._0, t2._0);
  }
}

var A = {
  compare
};

var ASet = $$Set.Make(A);

var X0 = {};

var Y0 = {};

function f(x) {
  return x + 1 | 0;
}

var X1 = {
  f
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
