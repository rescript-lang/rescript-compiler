'use strict';


function f(x) {
  var v = x;
  var sum = 0;
  while(v > 0) {
    sum = sum + v | 0;
    v = v - 1 | 0;
  };
  return sum;
}

exports.f = f;
/* No side effect */
