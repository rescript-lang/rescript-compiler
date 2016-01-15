// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var CamlinternalLazy = require("../stdlib/camlinternalLazy");
var Pervasives = require("../stdlib/pervasives");

function a4(prim) {
  return [
          0,
          'File "test_primitive.ml", line 30, characters 9-19',
          prim
        ];
}

function a5(prim) {
  return [
          0,
          31,
          prim
        ];
}

function a6(prim) {
  return [
          0,
          [
            0,
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

var a = xxx.charCodeAt(0);

function u(b) {
  return b ? (Pervasives.print_int(1), 32) : 7;
}

function f2(h, b, _) {
  return h(b ? 32 : 7);
}

v[1] = 3.0;

var unboxed_x = /* array */[
  0,
  0
];

function gg(x) {
  x[0] = 0;
  return /* () */0;
}

function f(x) {
  return x.length;
}

function is_lazy_force(x) {
  var tag = Caml_obj_runtime.caml_obj_tag(x);
  return tag === 250 ? x[1] : (
            tag === 246 ? CamlinternalLazy.force_lazy_block(x) : x
          );
}

function fib(n) {
  if (1 < (n >>> 0)) {
    var fib1 = fib(n - 1);
    var fib2 = fib(n - 2);
    return fib1 + fib2 + 3;
  }
  else {
    return 1;
  }
}

var a0 = 'File "test_primitive.ml", line 26, characters 9-16';

var a1 = "test_primitive.ml";

var a2 = 28;

var a3 = "Test_primitive";

var xx = [
  /* tuple */0,
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
