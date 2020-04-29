'use strict';


var u = 3;

function f(param) {
  throw {
        ExceptionID: -3,
        _1: "hi",
        Debug: "Invalid_argument"
      };
}

exports.u = u;
exports.f = f;
/* No side effect */
