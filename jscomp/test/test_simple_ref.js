'use strict';

var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");

var v = /* record */{
  contents: 0
};

function gen(param) {
  Pervasives.incr(v);
  return v.contents;
}

var h = /* record */{
  contents: 0
};

var a = 0;

var c = /* record */{
  contents: 0
};

var not_real_escape = a;

function real_escape(f, v) {
  return Curry._1(f, c);
}

var u = h;

exports.u = u;
exports.gen = gen;
exports.not_real_escape = not_real_escape;
exports.real_escape = real_escape;
/* No side effect */
