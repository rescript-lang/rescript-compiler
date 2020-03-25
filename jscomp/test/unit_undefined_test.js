'use strict';


function f_01(param) {
  return hi((function () {
                console.log("x");
                
              }));
}

function u(x) {
  if (x > 3) {
    return 1;
  } else if (x < 2) {
    return 2;
  } else if (x > 4) {
    return 0;
  } else {
    return 3;
  }
}

exports.f_01 = f_01;
exports.u = u;
/* No side effect */
