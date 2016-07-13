'use strict';


function f(x, y) {
  return x + y | 0;
}

exports.f = f;
/* No side effect */
