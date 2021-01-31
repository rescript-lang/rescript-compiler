'use strict';


function u(b) {
  if (b === 0) {
    return 0;
  } else {
    return 1;
  }
}

function u1(b) {
  return b === 0;
}

function u2(b) {
  return b === 0;
}

function u3(b) {
  if (b === 0) {
    return 3;
  } else {
    return 4;
  }
}

function u4(b) {
  if (b === 0) {
    return 3;
  } else {
    return 4;
  }
}

function u5(b) {
  return b !== 0;
}

function u6(b) {
  return b === 0;
}

exports.u = u;
exports.u1 = u1;
exports.u2 = u2;
exports.u3 = u3;
exports.u4 = u4;
exports.u5 = u5;
exports.u6 = u6;
/* No side effect */
