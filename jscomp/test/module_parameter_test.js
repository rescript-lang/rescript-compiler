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
  $$String.index_opt,
  $$String.rindex,
  $$String.rindex_opt,
  $$String.index_from,
  $$String.index_from_opt,
  $$String.rindex_from,
  $$String.rindex_from_opt,
  $$String.contains,
  $$String.contains_from,
  $$String.rcontains_from,
  $$String.uppercase,
  $$String.lowercase,
  $$String.capitalize,
  $$String.uncapitalize,
  $$String.uppercase_ascii,
  $$String.lowercase_ascii,
  $$String.capitalize_ascii,
  $$String.uncapitalize_ascii,
  $$String.compare,
  $$String.equal,
  $$String.split_on_char
];

var N = /* module */[/* s */s];

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
