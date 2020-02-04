'use strict';


function f1(x) {
  return x(/* () */0);
}

function f2(x, y) {
  return x(y, /* () */0);
}

exports.f1 = f1;
exports.f2 = f2;
/* No side effect */
