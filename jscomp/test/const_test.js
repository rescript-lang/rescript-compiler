'use strict';

var Block = require("../../lib/js/block.js");

function f(x) {
  return x;
}

function ff(x) {
  return x;
}

function fff(x) {
  var match = /* A */Block.__(0, [x]);
  switch (match.tag | 0) {
    case 0 : 
        return x;
    case 1 : 
        return 1;
    case 2 : 
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

function hh() {
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
