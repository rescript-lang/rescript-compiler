'use strict';

var Curry = require("../../lib/js/curry.js");

function f(param) {
  if (typeof param === "number") {
    if (param === /* G */0) {
      return 4;
    } else {
      return 5;
    }
  }
  switch (param.tag | 0) {
    case /* A */0 :
        return 0;
    case /* B */1 :
        return 1;
    case /* C */2 :
        return 2;
    case /* F */3 :
        return 3;
    
  }
}

function bind(x, f) {
  if (x.tag) {
    return x;
  } else {
    return {
            tag: /* Left */0,
            _0: Curry._1(f, x._0)
          };
  }
}

exports.f = f;
exports.bind = bind;
/* No side effect */
