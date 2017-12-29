'use strict';

var Curry = require("../../lib/js/curry.js");

var v = [0];

function gen() {
  v[0] = v[0] + 1 | 0;
  return v[0];
}

var h = [0];

var a = 0;

var c = [0];

var not_real_escape = a;

function real_escape(f, _) {
  return Curry._1(f, c);
}

var u = h;

exports.u = u;
exports.gen = gen;
exports.not_real_escape = not_real_escape;
exports.real_escape = real_escape;
/* No side effect */
