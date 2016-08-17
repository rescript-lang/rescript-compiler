'use strict';


function f(x) {
  x.pushState(3, "x");
  return x.pushState(/* None */0, "x");
}

exports.f = f;
/* No side effect */
