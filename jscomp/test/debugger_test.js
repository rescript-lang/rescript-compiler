'use strict';


function f(x, y) {
  console.log(/* tuple */[
        x,
        y
      ]);
  return x + y | 0;
}

function g() {
  f(1, 2);
  debugger;
  f(1, 2);
  debugger;
  return 3;
}

function exterme_g() {
  f(1, 2);
  debugger;
  var v = /* () */0;
  console.log(v);
  f(1, 2);
  debugger;
  return 3;
}

exports.f = f;
exports.g = g;
exports.exterme_g = exterme_g;
/* No side effect */
