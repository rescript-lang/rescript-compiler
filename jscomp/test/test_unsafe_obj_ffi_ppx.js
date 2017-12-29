'use strict';


function f(x) {
  return x.length + x.width;
}

function h(x) {
  x.height = 3;
  x.width = 3;
  return /* () */0;
}

function chain(x) {
  return x.element.length + x.element.length | 0;
}

function g(x) {
  x.method1(3);
  return x.method2(3, 3);
}

exports.f = f;
exports.h = h;
exports.chain = chain;
exports.g = g;
/* No side effect */
