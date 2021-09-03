'use strict';


function f(x) {
  x.pushState(3, "x");
  x.pushState(undefined, "x");
}

exports.f = f;
/* No side effect */
