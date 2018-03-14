'use strict';

var Curry = require("../../lib/js/curry.js");

function t0(x, f) {
  return Curry._1(f, Curry._1(f, Curry._1(f, x)));
}

function t1(x, f) {
  return Curry._1(f, x);
}

function t2(x, f, g) {
  return Curry._2(f, Curry._3(g, Curry._1(f, x), x, x), x);
}

function t3(x, f) {
  return Curry._3(f, x, 1, 2);
}

exports.t0 = t0;
exports.t1 = t1;
exports.t2 = t2;
exports.t3 = t3;
/* No side effect */
