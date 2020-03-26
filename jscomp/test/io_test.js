'use strict';


function f(param) {
  console.error("x");
  console.log(undefined);
  console.log("hi");
  console.log(undefined);
  
}

exports.f = f;
/* No side effect */
