'use strict';


var Test = {};

var u = {
  s: "hello"
};

function f(s, y) {
  var tmp = {};
  if (s !== undefined) {
    tmp.s = s;
  }
  console.log(tmp);
  console.log(y);
  
}

function make(s) {
  if (s !== undefined) {
    return s;
  } else {
    return null;
  }
}

var H = {
  make: make
};

exports.Test = Test;
exports.u = u;
exports.f = f;
exports.H = H;
/* No side effect */
