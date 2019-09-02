'use strict';


function f(x) {
  return x;
}

function ff(x) {
  return x;
}

function fff(x) {
  var match = /* constructor */{
    tag: "A",
    Arg0: x
  };
  switch (/* XXX */match.tag) {
    case "A" :
        return x;
    case "B" :
        return 1;
    case "C" :
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
