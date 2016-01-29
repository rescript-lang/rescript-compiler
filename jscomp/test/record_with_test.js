// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Mt = require("./mt");

var v = [
  /* record */0,
  /* None */0,
  0,
  0,
  0,
  0,
  0,
  0
];

var newrecord = v.slice();

newrecord[2] = 0;

function f(g, h) {
  var newrecord = g(h).slice();
  newrecord[2] = 0;
  return newrecord;
}

var suites_001 = [
  /* tuple */0,
  "eq_with",
  function () {
    return [
            /* Eq */0,
            v,
            newrecord
          ];
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_pair_suites("record_with_test.ml", suites);

var uv = [
  /* record */0,
  /* None */0,
  1,
  0,
  0,
  0,
  0,
  0
];

var u_v = newrecord;

exports.v      = v;
exports.uv     = uv;
exports.u_v    = u_v;
exports.f      = f;
exports.suites = suites;
/*  Not a pure module */
