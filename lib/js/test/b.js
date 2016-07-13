'use strict';


function f(point) {
  var y = point[/* y */1];
  var x = point[/* x */0];
  return Math.pow(x * x + y * y, 2);
}

exports.f = f;
/* No side effect */
