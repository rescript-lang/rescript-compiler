'use strict';


function test1(r) {
  var x = r.f;
  if (x !== undefined) {
    return x.f;
  }
  
}

exports.test1 = test1;
/* No side effect */
