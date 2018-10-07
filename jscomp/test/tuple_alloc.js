'use strict';

var Curry = require("../../lib/js/curry.js");

var v = /* record */[/* contents */0];

function reset(param) {
  v[0] = 0;
  return /* () */0;
}

function incr(param) {
  v[0] = v[0] + 1 | 0;
  return /* () */0;
}

var vv = /* record */[/* contents */0];

function reset2(param) {
  vv[0] = 0;
  return /* () */0;
}

function incr2(param) {
  v[0] = v[0] + 1 | 0;
  return /* () */0;
}

function f(a, b, d, e) {
  var h = Curry._1(a, b);
  var u = Curry._1(d, h);
  var v = Curry._1(e, h);
  return u + v | 0;
}

function kf(cb, v) {
  Curry._1(cb, v);
  return v + v | 0;
}

function ikf(v) {
  return kf((function (prim) {
                return /* () */0;
              }), v);
}

exports.v = v;
exports.reset = reset;
exports.incr = incr;
exports.reset2 = reset2;
exports.incr2 = incr2;
exports.f = f;
exports.kf = kf;
exports.ikf = ikf;
/* No side effect */
