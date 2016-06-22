// GENERATED CODE BY BUCKLESCRIPT VERSION 0.6.1 , PLEASE EDIT WITH CARE
'use strict';


function f(x, y) {
  return /* tuple */[
          x < y,
          x <= y,
          x > y,
          x >= y
        ];
}

function ff(x, y) {
  if (x < y) {
    return 1;
  }
  else {
    return 2;
  }
}

exports.f  = f;
exports.ff = ff;
/* No side effect */
