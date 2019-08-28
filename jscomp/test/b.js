'use strict';


function f(point) {
  var y = point.y;
  var x = point.x;
  return Math.pow(x * x + y * y, 2);
}

exports.f = f;
/* No side effect */
