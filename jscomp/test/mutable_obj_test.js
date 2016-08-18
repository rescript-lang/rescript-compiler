'use strict';


function f(x) {
  return x.height = 3;
}

exports.f = f;
/* No side effect */
