'use strict';


function a(a0, a1, a2, a3, a4) {
  return (((((1 + a0 | 0) + a1 | 0) + a2 | 0) + a3 | 0) + a4 | 0) + 2 | 0;
}

function b(a0, a1, a2, a3, a4) {
  return (((((1 + a0 | 0) + a1 | 0) + a2 | 0) + a3 | 0) + a4 | 0) + (((((1 + a0 | 0) + a1 | 0) + a2 | 0) + a3 | 0) + a4 | 0) | 0;
}

exports.a = a;
exports.b = b;
/* No side effect */
