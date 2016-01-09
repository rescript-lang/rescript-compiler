// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

function fib(n) {
  return 1 < (n >>> 0) ? fib(n - 1) + fib(n - 2) : 1;
}

function fib2(n) {
  return 1 < (-1 + n >>> 0) ? fib2(n - 1) + fib2(n - 2) : 1;
}

var v = 0;

for(var i = 0; i<= 10; ++i){
  v += i;
}

var sum = v;

var v$1 = 0;

for(var i$1 = 10; i$1>= 0; --i$1){
  v$1 += i$1;
}

var sumdown = v$1;

function cons(x, y) {
  return [
          /* Cons */0,
          x,
          y
        ];
}

function length(x) {
  return x ? 1 + length(x[2]) : 0;
}

function map(f, x) {
  return x ? [
            /* Cons */0,
            f(x[1]),
            map(f, x[2])
          ] : /* Nil */0;
}

function f(x) {
  var v = x;
  var sum = 0;
  while(v > 0) {
    sum += v;
    -- v;
  };
  return sum;
}

function fib3(n) {
  var fib_help = function (_a, _b, _n) {
    while(/* true */1) {
      var n = _n;
      var b = _b;
      var a = _a;
      if (n > 0) {
        _n = n - 1;
        _b = a + b;
        _a = b;
      }
      else {
        return a;
      }
    };
  };
  return fib_help(0, 1, n);
}

var b = fib;

exports.fib = fib;
exports.fib2 = fib2;
exports.b = b;
exports.sum = sum;
exports.sumdown = sumdown;
exports.cons = cons;
exports.length = length;
exports.map = map;
exports.f = f;
exports.fib3 = fib3;
/*  Not a pure module */
