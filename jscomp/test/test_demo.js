// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var List = require("../stdlib/list");

function fib(n) {
  return 1 < (-1 + n >>> 0) ? fib(n - 1) + fib(n - 2) : 1;
}

function cons(x, y) {
  return [
          /* Cons */0,
          x,
          y
        ];
}

function map(f, param) {
  return param ? [
            /* Cons */0,
            f(param[1]),
            map(f, param[2])
          ] : /* Nil */0;
}

function sum(n) {
  var v = 0;
  for(var i = 0; i<= n; ++i){
    v += i;
  }
  return v;
}

function f(x, y, z) {
  return x + y + z;
}

function g(x, y) {
  var u = x + y;
  return function (z) {
    return u + z;
  };
}

function g1(x, y) {
  var u = x + y;
  return function (xx, yy) {
    return xx + yy + u;
  };
}

var x = g(3, 5)(6);

var v = g1(3, 4)(6);

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
