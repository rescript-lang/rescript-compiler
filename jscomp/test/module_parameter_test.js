// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt       = require("./mt");
var $$String = require("../stdlib/string");

function u(v) {
  return v;
}

var s = [
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

var N = /* module */[s];

var v0 = 1;

function v(x) {
  return x.length;
}

var suites_000 = /* tuple */[
  "const",
  function () {
    return /* Eq */{
            0: 1,
            1: v0,
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "other",
    function () {
      return /* Eq */{
              0: 3,
              1: v("abc"),
              length: 2,
              tag: 0
            };
    }
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("module_parameter_test.ml", suites);

exports.u      = u;
exports.N      = N;
exports.v0     = v0;
exports.v      = v;
exports.suites = suites;
/*  Not a pure module */
