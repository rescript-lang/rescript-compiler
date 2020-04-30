'use strict';


function f(param) {
  throw {
        ExceptionID: "Assert_failure",
        _1: /* tuple */[
          "noassert.ml",
          5,
          11
        ]
      };
}

function h(param) {
  
}

exports.f = f;
exports.h = h;
/* No side effect */
