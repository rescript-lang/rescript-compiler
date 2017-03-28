'use strict';


function u(x, y) {
  return x + y | 0;
}

function max2(x, y) {
  return x + y;
}

var hh = 1 + 2;

var h = 3;

exports.u    = u;
exports.h    = h;
exports.max2 = max2;
exports.hh   = hh;
/* No side effect */
