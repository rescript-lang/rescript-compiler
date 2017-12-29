'use strict';

var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

function f(param) {
  if (typeof param === "number") {
    if (param) {
      return 5;
    } else {
      return 4;
    }
  } else {
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
  } else {
    return /* Left */Block.__(0, [Curry._1(f, x[0])]);
  }
}

exports.f = f;
exports.bind = bind;
/* No side effect */
