'use strict';

var $$Array    = require("../../lib/js/array.js");
var Caml_array = require("../../lib/js/caml_array.js");

function f(a, b, _) {
  return a + b | 0;
}

function f2(a) {
  return function () {
    return a + 1 | 0;
  };
}

var arr = $$Array.init(2, function () {
      return 0;
    });

for(var i = 0; i <= 1; ++i){
  Caml_array.caml_array_set(arr, i, i + 1 | 0);
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
