'use strict';


function f0(x) {
  var match = x[1];
  if (match !== /* None */0 && match[0]) {
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

function f2(x, y, $staropt$star, _) {
  var z = $staropt$star !== /* None */0 ? $staropt$star[0] : 3;
  console.log(x);
  if (y !== /* None */0) {
    return y[0] + z | 0;
  } else {
    return 0;
  }
}

function f3(x) {
  if (x !== /* None */0) {
    return 1;
  } else {
    return 0;
  }
}

function f4(x) {
  if (x !== /* None */0) {
    return x[0] + 1 | 0;
  } else {
    return 0;
  }
}

function f5() {
  return false;
}

function f6() {
  return true;
}

exports.f0 = f0;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
/* No side effect */
