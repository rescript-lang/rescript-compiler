'use strict';


function f(x) {
  if (x !== 1) {
    if (x !== 2) {
      return x + 3;
    } else {
      return 0;
    }
  } else {
    return 0;
  }
}

exports.f = f;
/* No side effect */
