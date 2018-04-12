'use strict';


function f(x) {
  return x + (1) | 0;
}

var a = f(1);

var b = f(2);

function g() {
  return (1);
}

var c = g(/* () */0);

var d = g(/* () */0);

exports.f = f;
exports.a = a;
exports.b = b;
exports.g = g;
exports.c = c;
exports.d = d;
/* a Not a pure module */
