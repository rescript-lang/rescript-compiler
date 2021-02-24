'use strict';


function f(x) {
  x.height = 3;
  
}

function h(x) {
  return x.height;
}

exports.f = f;
exports.h = h;
/* No side effect */
