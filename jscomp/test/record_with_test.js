// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Caml_curry = require("../runtime/caml_curry");

var v = /* record */[
  /* None */0,
  0,
  0,
  0,
  0,
  0,
  0
];

var newrecord = v.slice();

newrecord[1] = 0;

function f(g, h) {
  var newrecord = Caml_curry.app1(g, h).slice();
  newrecord[1] = 0;
  return newrecord;
}

var suites_000 = /* tuple */[
  "eq_with",
  function () {
    return /* Eq */{
            0: v,
            1: newrecord,
            length: 2,
            tag: 0
          };
  }
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("record_with_test.ml", suites);

var uv = /* record */[
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
