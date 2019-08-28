'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_splice_call = require("../../lib/js/caml_splice_call.js");

var Curry$1 = { };

var Block = { };

var xbs = Array.prototype.map.call(/* array */[
      1,
      2,
      3,
      5
    ], (function (x) {
        return x + 1 | 0;
      }));

function f(cb) {
  return Array.prototype.map.call(/* array */[
              1,
              2,
              3,
              4
            ], Curry.__1(cb));
}

var xs = Array.prototype.map.call(/* array */[
      1,
      1,
      2
    ], (function (x) {
        return (function (y) {
            return (y + x | 0) + 1 | 0;
          });
      }));

function f_0(param) {
  return hi((function () {
                return /* () */0;
              }));
}

function f_01(param) {
  return hi((function () {
                console.log("x");
                return /* () */0;
              }));
}

function f_02(xs) {
  return hi((function () {
                xs.contents = /* () */0;
                console.log("x");
                return /* () */0;
              }));
}

function f_03(xs, u) {
  return hi((function () {
                return Curry._1(u, /* () */0);
              }));
}

function fishy(x, y, z) {
  return map2(x, y, Curry.__2(z));
}

function h(x, y, z) {
  return map2(x, y, Curry.__2(z));
}

function h1(x, y, u, z) {
  var partial_arg = Curry._1(z, u);
  return map2(x, y, Curry.__2(partial_arg));
}

function add3(x, y, z) {
  return (x + y | 0) + z | 0;
}

function h2(x) {
  return ff(x, (function (prim, prim$1) {
                return prim + prim$1 | 0;
              }));
}

function h3(x) {
  return ff(x, (function (param, param$1) {
                return add3(1, param, param$1);
              }));
}

function h4(x) {
  return ff1(x, 3, (function (param, param$1) {
                return add3(1, param, param$1);
              }));
}

function h5(x) {
  return ff2(x, "3", (function (param, param$1) {
                return add3(2, param, param$1);
              }));
}

function add(x, y) {
  console.log(/* tuple */[
        x,
        y
      ]);
  return x + y | 0;
}

function h6(x) {
  return ff2(x, "3", add);
}

function unit_magic(param) {
  console.log("noinline");
  console.log("noinline");
  return 3;
}

var f_unit_magic = unit_magic(/* () */0);

function hh(xs) {
  return (function (param) {
      Caml_splice_call.spliceApply(f_0002, [
            xs,
            param
          ]);
      return /* () */0;
    });
}

exports.Curry = Curry$1;
exports.Block = Block;
exports.xbs = xbs;
exports.f = f;
exports.xs = xs;
exports.f_0 = f_0;
exports.f_01 = f_01;
exports.f_02 = f_02;
exports.f_03 = f_03;
exports.fishy = fishy;
exports.h = h;
exports.h1 = h1;
exports.add3 = add3;
exports.h2 = h2;
exports.h3 = h3;
exports.h4 = h4;
exports.h5 = h5;
exports.add = add;
exports.h6 = h6;
exports.unit_magic = unit_magic;
exports.f_unit_magic = f_unit_magic;
exports.hh = hh;
/* xbs Not a pure module */
