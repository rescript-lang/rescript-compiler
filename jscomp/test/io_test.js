'use strict';


function f() {
  var v = (console.error("x"), /* () */0);
  console.log(v);
  var u = (console.log("hi"), /* () */0);
  console.log(u);
  return /* () */0;
}

exports.f = f;
/* No side effect */
