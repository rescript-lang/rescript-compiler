'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

console.log(3);

var A = Caml_exceptions.create("Internal_unused_test.P1.A");

function f(param) {
  throw {
        RE_EXN_ID: A,
        Error: new Error()
      };
}

var c = 5;

function h(a, b) {
  return a + b | 0;
}

var h1 = 2;

var h2 = h1 + 1 | 0;

var h4 = 2;

var h5 = h4 + 1 | 0;

var b = 5;

var N = {
  b
};

console.log(h5);

console.log(h2);

console.log(c);

console.log(h(1, 2));

exports.f = f;
exports.N = N;
/*  Not a pure module */
