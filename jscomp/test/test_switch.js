'use strict';

var Curry = require("../../lib/js/curry.js");

function f(param) {
  if (typeof param === "string") {
    if (param === "G") {
      return 4;
    } else {
      return 5;
    }
  } else {
    switch (/* XXX */param.tag) {
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
}

function bind(x, f) {
  if (/* XXX */x.tag === "Left") {
    return /* constructor */{
            tag: "Left",
            Arg0: Curry._1(f, x.Arg0)
          };
  } else {
    return x;
  }
}

exports.f = f;
exports.bind = bind;
/* No side effect */
