'use strict';

var Curry = require("../../lib/js/curry.js");

function $pipe$great(x, f) {
  return Curry._1(f, x);
}

function hello1(y, f) {
  return Curry._1(f, y);
}

function hello2(y, f) {
  return Curry._1(f, y);
}

function hello3(y, f) {
  return Curry._1(f, y);
}

function hello4(y, f) {
  return Curry._1(f, y);
}

function hello5(y, f) {
  return Curry._1(f, y);
}

function f(_x) {
  while(true) {
    var x = _x;
    _x = (((x + 1 | 0) + 1 | 0) + 1 | 0) + 1 | 0;
    continue ;
  };
}

function ff(_x, _y) {
  while(true) {
    var y = _y;
    var x = _x;
    _y = x + 1 | 0;
    _x = y;
    continue ;
  };
}

function fff(_, _$1) {
  while(true) {
    continue ;
  };
}

exports.$pipe$great = $pipe$great;
exports.hello1 = hello1;
exports.hello2 = hello2;
exports.hello3 = hello3;
exports.hello4 = hello4;
exports.hello5 = hello5;
exports.f = f;
exports.ff = ff;
exports.fff = fff;
/* No side effect */
