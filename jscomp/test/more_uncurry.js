'use strict';


function f(x, y, z) {
  if (z !== undefined) {
    return (x + y | 0) + z | 0;
  } else {
    return x + y | 0;
  }
}

exports.f = f;
/* No side effect */
