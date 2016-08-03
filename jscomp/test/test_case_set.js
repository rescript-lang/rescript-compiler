'use strict';

var Curry = require("../../lib/js/curry");

function f(x) {
  return x.case = 3;
}

function g(x) {
  return Curry.js2(-977287917, 1, x, 3);
}

exports.f = f;
exports.g = g;
/* No side effect */
