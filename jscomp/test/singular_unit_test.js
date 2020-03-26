'use strict';


function f0(x) {
  return x;
}

function f1(x) {
  return 2;
}

function f3(x) {
  if (x !== undefined) {
    return x;
  } else {
    return /* A */0;
  }
}

var v0;

exports.f0 = f0;
exports.f1 = f1;
exports.f3 = f3;
exports.v0 = v0;
/* No side effect */
