'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");

function h0(x) {
  return x();
}

function h00(x) {
  return Curry._1(x(), /* () */0);
}

function h1(x) {
  return (function (param) {
      return x(param);
    });
}

function h10(x) {
  return x(3);
}

function h30(x) {
  return (function (param) {
      return x(3, 3, param);
    });
}

function h33(x) {
  return x(1, 2, 3);
}

function h34(x) {
  return Curry._1(x(1, 2, 3), 4);
}

function ocaml_run(param, param$1) {
  return (function (x, y, z) {
              return (x + y | 0) + z | 0;
            })(1, param, param$1);
}

function a0() {
  console.log("hi");
  return /* () */0;
}

function a1() {
  return (function (x) {
      return x;
    });
}

function a2(x, y) {
  return x + y | 0;
}

function a3(x, y, z) {
  return (x + y | 0) + z | 0;
}

function a4(x, y, z, param) {
  var u = (Caml_int32.imul(x, x) + Caml_int32.imul(y, y) | 0) + Caml_int32.imul(z, z) | 0;
  return (function (d) {
              return u + d | 0;
            })(param);
}

function a44(x, y, z, d) {
  var u = (Caml_int32.imul(x, x) + Caml_int32.imul(y, y) | 0) + Caml_int32.imul(z, z) | 0;
  return u + d | 0;
}

function b44() {
  return (function (x, y, z, d) {
      return /* tuple */[
              x,
              y,
              z,
              d
            ];
    });
}

function xx() {
  return (function () {
      console.log(3);
      return /* () */0;
    });
}

var test_as = List.map;

exports.h0 = h0;
exports.h00 = h00;
exports.h1 = h1;
exports.h10 = h10;
exports.h30 = h30;
exports.h33 = h33;
exports.h34 = h34;
exports.ocaml_run = ocaml_run;
exports.a0 = a0;
exports.a1 = a1;
exports.a2 = a2;
exports.a3 = a3;
exports.a4 = a4;
exports.a44 = a44;
exports.b44 = b44;
exports.test_as = test_as;
exports.xx = xx;
/* No side effect */
