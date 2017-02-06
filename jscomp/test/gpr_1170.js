'use strict';


function f(resp) {
  resp.statusCode = 200;
  return resp.hi = "hi";
}

exports.f = f;
/* No side effect */
