// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

function f(param) {
  if (typeof param === "number") {
    return param ? 5 : 4;
  }
  else {
    switch (param[0]) {
      case 0 : 
          return 0;
      case 1 : 
          return 1;
      case 2 : 
          return 2;
      case 3 : 
          return 3;
      
    }
  }
}

function bind(x, f) {
  return x[0] ? x : [
            /* Left */0,
            f(x[1])
          ];
}

exports.f = f;
exports.bind = bind;
/* No side effect */
