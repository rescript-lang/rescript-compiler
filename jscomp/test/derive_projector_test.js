'use strict';

var Block = require("../../lib/js/block.js");

function u_x(param) {
  return param.u_x;
}

function b_x(param) {
  return param.b_x;
}

function c_x(param) {
  return param.c_x;
}

function d_int(param_0) {
  return {
          tag: /* D_int */0,
          _0: param_0
        };
}

function d_tuple(param_0, param_1) {
  return {
          tag: /* D_tuple */1,
          _0: param_0,
          _1: param_1
        };
}

function newContent(param_0) {
  return {
          tag: /* NewContent */2,
          _0: param_0
        };
}

function d_tweak(param_0) {
  return {
          tag: /* D_tweak */3,
          _0: param_0
        };
}

function u_X(param) {
  return param.u_X;
}

function d(param) {
  return param.d;
}

var v = {
  tag: /* D_int */0,
  _0: 3
};

var h_1 = /* :: */{
  _0: {
    tag: /* D_int */0,
    _0: 3
  },
  _1: /* :: */{
    _0: {
      tag: /* D_tuple */1,
      _0: 3,
      _1: "hgo"
    },
    _1: /* :: */{
      _0: {
        tag: /* D_tweak */3,
        _0: /* tuple */[
          3,
          "hgo"
        ]
      },
      _1: /* :: */{
        _0: {
          tag: /* NewContent */2,
          _0: "3"
        },
        _1: /* [] */0
      }
    }
  }
};

var h = /* :: */{
  _0: /* D_empty */0,
  _1: h_1
};

function xx(param_0) {
  return /* Xx */{
          _0: param_0
        };
}

function a(param_0) {
  return /* A */{
          _0: param_0
        };
}

var d_empty = /* D_empty */0;

var hei = /* Hei */0;

exports.u_x = u_x;
exports.b_x = b_x;
exports.c_x = c_x;
exports.d_empty = d_empty;
exports.d_int = d_int;
exports.d_tuple = d_tuple;
exports.newContent = newContent;
exports.d_tweak = d_tweak;
exports.hei = hei;
exports.u_X = u_X;
exports.d = d;
exports.v = v;
exports.h = h;
exports.xx = xx;
exports.a = a;
/* No side effect */
