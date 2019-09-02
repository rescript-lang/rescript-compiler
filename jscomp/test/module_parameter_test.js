'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var $$String = require("../../lib/js/string.js");

function u(v) {
  return v;
}

var s = $$String;

var N = {
  s: s
};

function v(x) {
  return x.length;
}

var suites_000 = /* tuple */[
  "const",
  (function (param) {
      return /* Eq */Block.__(0, [
                1,
                1
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "other",
    (function (param) {
        return /* Eq */Block.__(0, [
                  3,
                  3
                ]);
      })
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("Module_parameter_test", suites);

var v0 = 1;

exports.u = u;
exports.N = N;
exports.v0 = v0;
exports.v = v;
exports.suites = suites;
/*  Not a pure module */
