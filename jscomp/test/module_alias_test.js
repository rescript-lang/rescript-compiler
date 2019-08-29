'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function f(x) {
  var L = List;
  console.log(x);
  console.log(List.length(x));
  return L;
}

var h = f(/* [] */0);

var a = Curry._1(h.length, /* :: */[
      1,
      /* :: */[
        2,
        /* :: */[
          3,
          /* [] */0
        ]
      ]
    ]);

eq("File \"module_alias_test.ml\", line 30, characters 6-13", a, 3);

Mt.from_pair_suites("Module_alias_test", suites[0]);

var N = 0;

var V = 0;

var J = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.N = N;
exports.V = V;
exports.J = J;
exports.f = f;
exports.a = a;
/* h Not a pure module */
