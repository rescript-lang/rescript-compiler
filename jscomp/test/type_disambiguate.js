'use strict';


var N = {};

function f(e) {
  return (e.a + e.b | 0) + e.c | 0;
}

function f1(e) {
  var c = e.c;
  return ((e.a + e.b | 0) + c | 0) + e.d(c) | 0;
}

exports.N = N;
exports.f = f;
exports.f1 = f1;
/* No side effect */
