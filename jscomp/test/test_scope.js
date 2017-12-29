'use strict';


var h = x(3);

var hh = x(3);

function f(x, y) {
  return x + y;
}

exports.h = h;
exports.hh = hh;
exports.f = f;
/* h Not a pure module */
