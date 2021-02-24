'use strict';


function f(x) {
  x.case = 3;
  
}

function g(x) {
  return x.item(3);
}

exports.f = f;
exports.g = g;
/* No side effect */
