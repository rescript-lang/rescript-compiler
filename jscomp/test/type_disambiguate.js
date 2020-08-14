'use strict';


var N = {};

function f(e) {
  return (e.a + e.b | 0) + e.c | 0;
}

exports.N = N;
exports.f = f;
/* No side effect */
