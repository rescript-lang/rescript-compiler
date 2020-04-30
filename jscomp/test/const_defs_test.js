'use strict';


var u = 3;

function f(param) {
  throw {
        ExceptionID: "Invalid_argument",
        _1: "hi"
      };
}

exports.u = u;
exports.f = f;
/* No side effect */
