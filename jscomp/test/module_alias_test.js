'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

function f(x) {
  console.log(x);
  console.log(List.length(x));
  return List;
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

Mt.from_pair_suites("Module_alias_test", suites.contents);

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
