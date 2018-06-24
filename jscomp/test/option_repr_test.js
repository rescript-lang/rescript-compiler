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

var f10 = /* Some */[/* Some */[/* Some */[/* Some */[/* None */0]]]];

var f11 = /* Some */[f10];

var f7 = /* None */0;

var f8 = /* Some */[/* None */0];

var f9 = /* Some */[/* Some */[/* None */0]];

var f12 = /* Some */[/* Some */[/* Some */[/* Some */[/* :: */[
          /* tuple */[
            1,
            2
          ],
          /* [] */0
        ]]]]];

exports.f0 = f0;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
exports.f9 = f9;
exports.f10 = f10;
exports.f11 = f11;
exports.f12 = f12;
/* No side effect */
