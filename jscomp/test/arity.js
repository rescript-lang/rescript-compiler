'use strict';

var Curry = require("../../lib/js/curry.js");

function u(f, a, b) {
  console.log(f(a, b));
  console.log(f(a, b));
  
}

function u2(f, a, b) {
  console.log(Curry._2(f, a, b));
  console.log(Curry._2(f, a, b));
  
}

function f(x, y) {
  return x + y | 0;
}

function add(prim, prim$1) {
  return prim + prim$1 | 0;
}

function h(u) {
  var m = u.hi;
  return m(1, 2);
}

var nested = {
  x: {
    y: 3
  }
};

exports.u = u;
exports.u2 = u2;
exports.f = f;
exports.add = add;
exports.h = h;
exports.nested = nested;
/* No side effect */
