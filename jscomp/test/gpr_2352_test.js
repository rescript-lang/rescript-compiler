'use strict';


function f(x) {
  x.hey = 22;
  return /* () */0;
}

exports.f = f;
/* No side effect */
