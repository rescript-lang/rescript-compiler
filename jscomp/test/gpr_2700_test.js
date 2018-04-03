'use strict';


function f(x) {
  if (x === 3) {
    return true;
  } else {
    return x === 4;
  }
}

exports.f = f;
/* No side effect */
