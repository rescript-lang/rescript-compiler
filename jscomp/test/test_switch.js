// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");

function f(param) {
  if (typeof param === "number") {
    if (param) {
      return 5;
    }
    else {
      return 4;
    }
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
  if (x[0]) {
    return x;
  }
  else {
    return [
            /* Left */0,
            Caml_curry.app1(f, x[1])
          ];
  }
}

exports.f    = f;
exports.bind = bind;
/* No side effect */
