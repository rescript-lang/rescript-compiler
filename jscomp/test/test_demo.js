'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

function fib(n) {
  if (n === 2 || n === 1) {
    return 1;
  } else {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  }
}

function cons(x, y) {
  return /* Cons */[
          x,
          y
        ];
}

function map(f, param) {
  if (param) {
    return /* Cons */[
            Curry._1(f, param[0]),
            map(f, param[1])
          ];
  } else {
    return /* Nil */0;
  }
}

function sum(n) {
  var v = 0;
  for(var i = 0; i <= n; ++i){
    v = v + i | 0;
  }
  return v;
}

function f(x, y, z) {
  return (x + y | 0) + z | 0;
}

function g(x, y) {
  var u = x + y | 0;
  return (function (z) {
      return u + z | 0;
    });
}

function g1(x, y) {
  var u = x + y | 0;
  return (function (xx, yy) {
      return (xx + yy | 0) + u | 0;
    });
}

var u = 8;

var x = (function (z) {
      return u + z | 0;
    })(6);

var u$1 = 7;

function v(param) {
  var xx = 6;
  var yy = param;
  return (xx + yy | 0) + u$1 | 0;
}

var nil = /* Nil */0;

var len = List.length;

exports.fib = fib;
exports.nil = nil;
exports.cons = cons;
exports.map = map;
exports.sum = sum;
exports.len = len;
exports.f = f;
exports.g = g;
exports.g1 = g1;
exports.x = x;
exports.v = v;
/* x Not a pure module */
