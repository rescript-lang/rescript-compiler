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
    switch (param.tag | 0) {
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
  if (x.tag) {
    return x;
  }
  else {
    return /* Left */{
            0: Caml_curry.app1(f, x[0]),
            length: 1,
            tag: 0
          };
  }
}

exports.f    = f;
exports.bind = bind;
/* No side effect */
