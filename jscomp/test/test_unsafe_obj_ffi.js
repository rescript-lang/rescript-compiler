'use strict';


function f(x) {
  return x.height + x.width | 0;
}

function g(x) {
  x.method1(3);
  return x.method2(3, 3);
}

function h(x) {
  x.height = 3;
  x.width = 3;
  return /* () */0;
}

exports.f = f;
exports.g = g;
exports.h = h;
/* No side effect */
