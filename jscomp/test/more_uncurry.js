'use strict';


function f(x, y, x_) {
  if (x_ !== undefined) {
    return (x + y | 0) + x_ | 0;
  } else {
    return x + y | 0;
  }
}

exports.f = f;
/* No side effect */
