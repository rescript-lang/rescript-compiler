'use strict';


function f() {
  console.error("x");
  console.log(/* () */0);
  console.log("hi");
  console.log(/* () */0);
  return /* () */0;
}

exports.f = f;
/* No side effect */
