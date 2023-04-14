'use strict';


function foo(x, y) {
  return x + y | 0;
}

var z = 7;

function bar(x, y) {
  return x + y | 0;
}

var b = 7;

var w = 7;

var a = 7;

console.log(a);

[1].map(function (x) {
      return x + 1 | 0;
    });

function ptl(param) {
  return foo(10, param);
}

function foo2(x, y) {
  return x + y | 0;
}

function bar2(__x) {
  return __x + 3 | 0;
}

function foo3(x, y, z) {
  return (x + y | 0) + z | 0;
}

function bar3(__x) {
  return foo3(__x, 3, 4);
}

function q(param) {
  return null;
}

function inl() {
  
}

function inl2(x, y) {
  return x + y | 0;
}

exports.foo = foo;
exports.z = z;
exports.bar = bar;
exports.b = b;
exports.w = w;
exports.a = a;
exports.ptl = ptl;
exports.foo2 = foo2;
exports.bar2 = bar2;
exports.foo3 = foo3;
exports.bar3 = bar3;
exports.q = q;
exports.inl = inl;
exports.inl2 = inl2;
/*  Not a pure module */
