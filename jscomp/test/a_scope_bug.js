// Generated CODE, PLEASE EDIT WITH CARE
"use strict";


function odd(_z) {
  while(true) {
    var z = _z;
    var even = z * z;
    var a = even + 4 + even;
    console.log("" + a);
    _z = 32;
  };
}

function even(y) {
  return odd(y);
}

exports.odd  = odd;
exports.even = even;
/* No side effect */
