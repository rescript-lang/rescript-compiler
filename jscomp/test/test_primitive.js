'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Pervasives = require("../../lib/js/pervasives.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

function a4(prim) {
  return [
          "File \"test_primitive.ml\", line 30, characters 9-19",
          prim
        ];
}

function a5(prim) {
  return [
          31,
          prim
        ];
}

function a6(prim) {
  return [
          /* tuple */[
            "test_primitive.ml",
            32,
            9,
            19
          ],
          prim
        ];
}

var test_float = 3;

var test_abs = Math.abs(3.0);

var v = /* array */[
  1.0,
  2.0
];

var xxx = "a";

var a = /* "a" */97;

function u(b) {
  if (b) {
    Pervasives.print_int(1);
    return 32;
  } else {
    return 7;
  }
}

function f2(h, b, _) {
  return Curry._1(h, b ? 32 : 7);
}

Caml_array.caml_array_set(v, 1, 3.0);

var unboxed_x = /* record */[
  /* u */0,
  /* v */0
];

function gg(x) {
  x[/* u */0] = 0;
  return /* () */0;
}

function f(x) {
  return x.length;
}

function is_lazy_force(x) {
  var tag = x.tag | 0;
  if (tag === 250) {
    return x[0];
  } else if (tag === 246) {
    return CamlinternalLazy.force_lazy_block(x);
  } else {
    return x;
  }
}

function fib(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    var fib1 = fib(n - 1 | 0);
    var fib2 = fib(n - 2 | 0);
    return (fib1 + fib2 | 0) + 3 | 0;
  }
}

var a0 = "File \"test_primitive.ml\", line 26, characters 9-16";

var a1 = "test_primitive.ml";

var a2 = 28;

var a3 = "Test_primitive";

var xx = /* tuple */[
  0,
  0
];

exports.a0 = a0;
exports.a1 = a1;
exports.a2 = a2;
exports.a3 = a3;
exports.a4 = a4;
exports.a5 = a5;
exports.a6 = a6;
exports.test_float = test_float;
exports.test_abs = test_abs;
exports.v = v;
exports.xxx = xxx;
exports.a = a;
exports.u = u;
exports.f2 = f2;
exports.xx = xx;
exports.unboxed_x = unboxed_x;
exports.gg = gg;
exports.f = f;
exports.is_lazy_force = is_lazy_force;
exports.fib = fib;
/*  Not a pure module */
