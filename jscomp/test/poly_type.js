'use strict';


function f(x) {
  x.pushState(3, "x");
  return x.pushState(undefined, "x");
}

exports.f = f;
/* No side effect */
