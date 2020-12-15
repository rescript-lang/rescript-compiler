'use strict';

var Curry = require("../../lib/js/curry.js");

function f(value) {
  if (value == null) {
    return ;
  } else {
    return value;
  }
}

function fxx(v) {
  var match = Curry._1(v, undefined);
  if (match > 3 || match < 1) {
    return /* "d" */100;
  }
  switch (match) {
    case 1 :
        return /* "a" */97;
    case 2 :
        return /* "b" */98;
    case 3 :
        return /* "c" */99;
    
  }
}

function fxxx2(v) {
  if (Curry._1(v, undefined)) {
    return 2;
  } else {
    return 1;
  }
}

function fxxx3(v) {
  if (Curry._1(v, undefined)) {
    return 2;
  } else {
    return 1;
  }
}

exports.f = f;
exports.fxx = fxx;
exports.fxxx2 = fxxx2;
exports.fxxx3 = fxxx3;
/* No side effect */
