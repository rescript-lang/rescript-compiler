'use strict';


function f(obj, x, y) {
  return obj.paint(x, y).draw(x, y).bark(x, y);
}

exports.f = f;
/* No side effect */
