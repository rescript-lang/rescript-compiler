// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let List = require("../../lib/js/list.js");

function fib(x) {
  if (x === 2 || x === 1) {
    return 1;
  } else {
    return fib(x - 1 | 0) + fib(x - 2 | 0) | 0;
  }
}

function cons(x, y) {
  return {
    TAG: "Cons",
    _0: x,
    _1: y
  };
}

function map(f, x) {
  if (typeof x !== "object") {
    return "Nil";
  } else {
    return {
      TAG: "Cons",
      _0: f(x._0),
      _1: map(f, x._1)
    };
  }
}

function sum(n) {
  let v = 0;
  for(let i = 0; i <= n; ++i){
    v = v + i | 0;
  }
  return v;
}

function f(x, y, z) {
  return (x + y | 0) + z | 0;
}

function g(x, y) {
  let u = x + y | 0;
  return function (z) {
    return u + z | 0;
  };
}

function g1(x, y) {
  let u = x + y | 0;
  return function (xx, yy) {
    return (xx + yy | 0) + u | 0;
  };
}

let u = 8;

let x = (function (z) {
  return u + z | 0;
})(6);

function v(extra) {
  let u = 7;
  return (6 + extra | 0) + u | 0;
}

let nil = "Nil";

let len = List.length;

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
