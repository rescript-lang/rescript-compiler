'use strict';


function f(x) {
  return x.height + x.width | 0;
}

function g(x) {
  x.method1(3);
  x.method2(3, 3);
}

exports.f = f;
exports.g = g;
/* No side effect */
