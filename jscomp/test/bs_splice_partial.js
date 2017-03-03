'use strict';


function test_hi(x) {
  var match = x.hi(1, 2, 3);
  if (match) {
    console.log(match[0]);
    return 2;
  }
  else {
    return 1;
  }
}

exports.test_hi = test_hi;
/* No side effect */
