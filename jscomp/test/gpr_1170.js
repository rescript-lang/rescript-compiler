'use strict';


function f(resp) {
  resp.statusCode = 200;
  resp.hi = "hi";
  return /* () */0;
}

exports.f = f;
/* No side effect */
