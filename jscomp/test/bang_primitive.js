'use strict';


function test(x, y) {
  return /* tuple */[
          +(x < y),
          +(x <= y),
          +(x > y),
          +(x >= y),
          +(x === y),
          +(x !== y)
        ];
}

exports.test = test;
/* No side effect */
