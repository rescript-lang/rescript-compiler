'use strict';

var Curry = require("../../lib/js/curry.js");

function u(x, x$1) {
  return x$1 + x$1 | 0;
}

function f(g, x) {
  var u = Curry._1(g, x);
  return u + u | 0;
}

exports.u = u;
exports.f = f;
/* No side effect */
