'use strict';

var Block = require("../../lib/js/block.js");

function f(x) {
  return x;
}

function ff(x) {
  return x;
}

function fff(x) {
  var x$1 = /* A */Block.__(0, [x]);
  switch (x$1.tag | 0) {
    case /* A */0 :
        return x;
    case /* B */1 :
        return 1;
    case /* C */2 :
        return 2;
    
  }
}

function h(x) {
  if (x !== 66) {
    if (x >= 67) {
      return 2;
    } else {
      return 0;
    }
  } else {
    return 1;
  }
}

function hh(param) {
  return 3;
}

var g = h(/* A */65);

exports.f = f;
exports.ff = ff;
exports.fff = fff;
exports.h = h;
exports.hh = hh;
exports.g = g;
/* g Not a pure module */
