'use strict';


function test(x, y) {
  return /* tuple */[
          x < y,
          x <= y,
          x > y,
          x >= y,
          x === y,
          x !== y
        ];
}

function f(x, _) {
  return /* tuple */[
          String.fromCharCode.apply((null), x),
          0
        ];
}

exports.test = test;
exports.f = f;
/* No side effect */
