'use strict';


function f(x) {
  return x.height + x.width | 0;
}

exports.f = f;
/* No side effect */
