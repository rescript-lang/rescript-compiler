'use strict';


function f(x, y, param) {
  if (param !== undefined) {
    return (x + y | 0) + param | 0;
  } else {
    return x + y | 0;
  }
}

exports.f = f;
/* No side effect */
