'use strict';


function f(param) {
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "noassert.ml",
          5,
          11
        ],
        Debug: "Assert_failure"
      };
}

function h(param) {
  
}

exports.f = f;
exports.h = h;
/* No side effect */
