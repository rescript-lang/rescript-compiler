'use strict';


function f0(x) {
  var match = x[1];
  if (match && match[0]) {
    return 1;
  } else {
    return 2;
  }
}

function f1(u) {
  if (u) {
    return 0;
  } else {
    return 1;
  }
}

exports.f0 = f0;
exports.f1 = f1;
/* No side effect */
