'use strict';

var Curry = require("../../lib/js/curry.js");

function t0(x, f) {
  return Curry._1(f, Curry._1(f, Curry._1(f, x)));
}

function t1(x, f) {
  return Curry._1(f, x);
}

function t2(x, f, g) {
  return Curry._2(f, Curry._3(g, Curry._1(f, x), x, x), x);
}

function t3(x, f) {
  return Curry._3(f, x, 1, 2);
}

function f(a, b, c) {
  return /* tuple */[
          Curry._1(b, a),
          Curry._1(c, a)
        ];
}

function f1(a, b, c, d) {
  var __ocaml_internal_obj = Curry._1(a, b);
  return /* tuple */[
          Curry._1(c, __ocaml_internal_obj),
          Curry._1(d, __ocaml_internal_obj)
        ];
}

function f2(a, b, c, d) {
  var __ocaml_internal_obj = Curry._1(a, b);
  var u = Curry._1(c, __ocaml_internal_obj);
  var v = Curry._1(d, __ocaml_internal_obj);
  return u + v | 0;
}

function f3(a, b, c, d) {
  var __ocaml_internal_obj = Curry._1(a, b);
  var u = Curry._1(c, __ocaml_internal_obj);
  var v = Curry._1(d, __ocaml_internal_obj);
  return u + v | 0;
}

function f4(a, b, c) {
  return /* tuple */[
          Curry._2(b, a, c),
          Curry._2(b, a, c)
        ];
}

exports.t0 = t0;
exports.t1 = t1;
exports.t2 = t2;
exports.t3 = t3;
exports.f = f;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
/* No side effect */
