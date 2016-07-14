'use strict';

var Curry = require("../../lib/js/curry");

function f(x) {
  return x.height = 3;
}

function h(x) {
  return Curry.js1(38537191, 1, x);
}

exports.f = f;
exports.h = h;
/* No side effect */
