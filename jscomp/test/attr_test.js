'use strict';


function u(x, y) {
  return x + y | 0;
}

var h = u(1, 2);

function max2(x, y) {
  return x + y;
}

var hh = max2(1, 2);

exports.u = u;
exports.h = h;
exports.max2 = max2;
exports.hh = hh;
/* h Not a pure module */
