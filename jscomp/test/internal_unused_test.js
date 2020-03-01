'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

console.log(3);

var A = Caml_exceptions.create("Internal_unused_test.P1.A");

function f(param) {
  throw A;
}

var c = 5;

function h(a, b) {
  return a + b | 0;
}

console.log(c);

console.log(h(1, 2));

exports.f = f;
/*  Not a pure module */
