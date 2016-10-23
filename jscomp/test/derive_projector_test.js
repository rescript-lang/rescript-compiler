'use strict';

var Block = require("../../lib/js/block");

function u_x(param) {
  return param[/* u_x */0];
}

function b_x(param) {
  return param[/* b_x */0];
}

function c_x(param) {
  return param[/* c_x */0];
}

function d_int(param_0) {
  return /* D_int */Block.__(0, [param_0]);
}

function d_tuple(param_0, param_1) {
  return /* D_tuple */Block.__(1, [
            param_0,
            param_1
          ]);
}

function d_tweak(param_0) {
  return /* D_tweak */Block.__(2, [param_0]);
}

function d(param) {
  return param[/* d */0];
}

var v = /* d : D_int */Block.__(0, [3]);

var d_empty = /* D_empty */0;

var hei = /* Hei */0;

exports.u_x     = u_x;
exports.b_x     = b_x;
exports.c_x     = c_x;
exports.d_empty = d_empty;
exports.d_int   = d_int;
exports.d_tuple = d_tuple;
exports.d_tweak = d_tweak;
exports.hei     = hei;
exports.d       = d;
exports.v       = v;
/* No side effect */
