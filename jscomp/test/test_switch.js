'use strict';

var Curry = require("../../lib/js/curry.js");

function f(param) {
  if (typeof param !== "object") {
    if (param === "G") {
      return 4;
    } else {
      return 5;
    }
  }
  switch (param.TAG) {
    case "A" :
        return 0;
    case "B" :
        return 1;
    case "C" :
        return 2;
    case "F" :
        return 3;
    
  }
}

function bind(x, f) {
  if (x.TAG === "Left") {
    return {
            TAG: "Left",
            _0: Curry._1(f, x._0)
          };
  } else {
    return x;
  }
}

exports.f = f;
exports.bind = bind;
/* No side effect */
