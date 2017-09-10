'use strict';

var Block = require("../../lib/js/block.js");

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

function newContent(param_0) {
  return /* NewContent */Block.__(2, [param_0]);
}

function d_tweak(param_0) {
  return /* D_tweak */Block.__(3, [param_0]);
}

function u_X(param) {
  return param[/* u_X */2];
}

function d(param) {
  return param[/* d */0];
}

var v = /* d : D_int */Block.__(0, [3]);

var h_001 = /* :: */[
  /* D_int */Block.__(0, [3]),
  /* :: */[
    /* D_tuple */Block.__(1, [
        3,
        "hgo"
      ]),
    /* :: */[
      /* D_tweak */Block.__(3, [/* tuple */[
            3,
            "hgo"
          ]]),
      /* :: */[
        /* NewContent */Block.__(2, ["3"]),
        /* [] */0
      ]
    ]
  ]
];

var h = /* :: */[
  /* D_empty */0,
  h_001
];

function xx(param_0) {
  return /* Xx */[param_0];
}

var d_empty = /* D_empty */0;

var hei = /* Hei */0;

exports.u_x        = u_x;
exports.b_x        = b_x;
exports.c_x        = c_x;
exports.d_empty    = d_empty;
exports.d_int      = d_int;
exports.d_tuple    = d_tuple;
exports.newContent = newContent;
exports.d_tweak    = d_tweak;
exports.hei        = hei;
exports.u_X        = u_X;
exports.d          = d;
exports.v          = v;
exports.h          = h;
exports.xx         = xx;
/* No side effect */
