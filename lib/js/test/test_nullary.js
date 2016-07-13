'use strict';


function f() {
  console.log("hey");
  return /* () */0;
}

exports.f = f;
/* No side effect */
