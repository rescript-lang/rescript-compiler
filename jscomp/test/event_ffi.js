// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_primitive = require("../runtime/caml_primitive");
var Caml_curry     = require("../runtime/caml_curry");

function h0(x) {
  return x();
}

function h00(x) {
  return Caml_curry.app1(x(), /* () */0);
}

function h1(x) {
  return function (param) {
    return x(param);
  };
}

function h10(x) {
  return x(3);
}

function h30(x) {
  return function (param) {
    return x(3, 3, param);
  };
}

function h33(x) {
  return x(1, 2, 3);
}

function h34(x) {
  return Caml_curry.app1(x(1, 2, 3), 4);
}

function ocaml_run(param, param$1) {
  var x = 1;
  var y = param;
  var z = param$1;
  return (x + y | 0) + z | 0;
}

function a0() {
  return Caml_curry.app1(function () {
              console.log("hi");
              return /* () */0;
            }, /* () */0);
}

function a1(x) {
  return x;
}

function a2(x, y) {
  return x + y | 0;
}

function a3(x, y, z) {
  return (x + y | 0) + z | 0;
}

function a4(param, param$1, param$2, param$3) {
  return Caml_curry.app4(function (x, y, z) {
              var u = (Caml_primitive.imul(x, x) + Caml_primitive.imul(y, y) | 0) + Caml_primitive.imul(z, z) | 0;
              return function (d) {
                return u + d | 0;
              };
            }, param, param$1, param$2, param$3);
}

function a44(x, y, z, d) {
  var u = (Caml_primitive.imul(x, x) + Caml_primitive.imul(y, y) | 0) + Caml_primitive.imul(z, z) | 0;
  return u + d | 0;
}

function b44(x, y, z, d) {
  return /* tuple */[
          x,
          y,
          z,
          d
        ];
}

exports.h0        = h0;
exports.h00       = h00;
exports.h1        = h1;
exports.h10       = h10;
exports.h30       = h30;
exports.h33       = h33;
exports.h34       = h34;
exports.ocaml_run = ocaml_run;
exports.a0        = a0;
exports.a1        = a1;
exports.a2        = a2;
exports.a3        = a3;
exports.a4        = a4;
exports.a44       = a44;
exports.b44       = b44;
/* partial_arg Not a pure module */
