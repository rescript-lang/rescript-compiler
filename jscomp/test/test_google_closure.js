// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let $$Array = require("../../lib/js/array.js");
let Caml_array = require("../../lib/js/caml_array.js");

function f(a, b, param) {
  return a + b | 0;
}

function f2(a) {
  return (extra) => {
    return a + 1 | 0;
  };
}

let a = String(3);

function f3(extra) {
  return 101;
}

let b = f3(2);

let arr = $$Array.init(2, ((param) => {
  return 0;
}));

for (let i = 0; i <= 1; ++i) {
  let f3$1 = (extra) => {
    return i + 1 | 0;
  };
  Caml_array.set(arr, i, f3$1(2));
}

console.log([
  a,
  b,
  arr
]);

let c = arr;

exports.f = f;
exports.f2 = f2;
exports.a = a;
exports.b = b;
exports.c = c;
/* a Not a pure module */
