// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("../stdlib/array");

function f(a, b, _) {
  return a + b;
}

function f2(a) {
  return function () {
    return a + 1;
  };
}

var arr = $$Array.init(2, function () {
      return 0;
    });

for(var i = 0; i<= 2; ++i){
  arr[i] = i + 1;
}

var match_000 = "" + 3;

var c = arr;

var b = 101;

var a = match_000;

console.log(/* tuple */[
      a,
      b,
      c
    ]);

exports.f  = f;
exports.f2 = f2;
exports.a  = a;
exports.b  = b;
exports.c  = c;
/* match Not a pure module */
