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

exports.foo = foo;
exports.z = z;
exports.bar = bar;
exports.b = b;
exports.w = w;
exports.a = a;
/*  Not a pure module */
