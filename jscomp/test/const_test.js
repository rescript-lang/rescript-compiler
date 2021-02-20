'use strict';


function f(x) {
  return x;
}

function ff(x) {
  return x;
}

function fff(x) {
  var x$1 = {
    TAG: /* A */0,
    _0: x
  };
  switch (x$1.TAG | 0) {
    case /* A */0 :
        return x;
    case /* B */1 :
        return 1;
    case /* C */2 :
        return 2;
    
  }
}

function h(x) {
  switch (x) {
    case "A" :
        return 0;
    case "B" :
        return 1;
    case "C" :
        return 2;
    
  }
}

function hh(param) {
  return 3;
}

var g = h("A");

exports.f = f;
exports.ff = ff;
exports.fff = fff;
exports.h = h;
exports.hh = hh;
exports.g = g;
/* g Not a pure module */
