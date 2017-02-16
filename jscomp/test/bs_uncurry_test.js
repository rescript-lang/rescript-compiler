'use strict';

var Curry = require("../../lib/js/curry");

var bs = Array.prototype.map.call(/* int array */[
      1,
      2,
      3,
      5
    ], function (x) {
      return x + 1 | 0;
    });

var xs = Array.prototype.map.call(/* int array */[
      1,
      1,
      2
    ], function (x) {
      return function (y) {
        return (y + x | 0) + 1 | 0;
      };
    });

function f(x, y, z) {
  return map2(x, y, function (param, param$1) {
              return Curry._2(Curry.__1(z), param, param$1);
            });
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
  return ff(x, function (prim, prim$1) {
              return prim + prim$1 | 0;
            });
}

function h3(x) {
  return ff(x, function (param, param$1) {
              return add3(1, param, param$1);
            });
}

exports.bs   = bs;
exports.xs   = xs;
exports.f    = f;
exports.h    = h;
exports.h1   = h1;
exports.add3 = add3;
exports.h2   = h2;
exports.h3   = h3;
/* bs Not a pure module */
