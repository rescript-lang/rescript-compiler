'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var $$String = require("../../lib/js/string.js");

function u(v) {
  return v;
}

var s = /* module */[
  $$String.make,
  $$String.init,
  $$String.copy,
  $$String.sub,
  $$String.fill,
  $$String.blit,
  $$String.concat,
  $$String.iter,
  $$String.iteri,
  $$String.map,
  $$String.mapi,
  $$String.trim,
  $$String.escaped,
  $$String.index,
  $$String.rindex,
  $$String.index_from,
  $$String.rindex_from,
  $$String.contains,
  $$String.contains_from,
  $$String.rcontains_from,
  $$String.uppercase,
  $$String.lowercase,
  $$String.capitalize,
  $$String.uncapitalize,
  $$String.compare
];

var N = /* module */[/* s */s];

function v(x) {
  return x.length;
}

var suites_000 = /* tuple */[
  "const",
  (function () {
      return /* Eq */Block.__(0, [
                1,
                1
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "other",
    (function () {
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

Mt.from_pair_suites("module_parameter_test.ml", suites);

var v0 = 1;

exports.u = u;
exports.N = N;
exports.v0 = v0;
exports.v = v;
exports.suites = suites;
/*  Not a pure module */
