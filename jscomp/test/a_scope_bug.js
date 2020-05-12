'use strict';


function odd(_z) {
  while(true) {
    var z = _z;
    var even = Math.imul(z, z);
    var a = (even + 4 | 0) + even | 0;
    console.log(String(a));
    _z = 32;
    continue ;
  };
}

var even = odd;

exports.odd = odd;
exports.even = even;
/* No side effect */
