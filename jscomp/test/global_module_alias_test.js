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

var v = {
  contents: 0
};

function Make(U) {
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  return U;
}

function f(param) {
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  return List;
}

eq("File \"global_module_alias_test.ml\", line 51, characters 5-12", List.length(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), 2);

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

var H = List;

eq("File \"global_module_alias_test.ml\", line 57, characters 5-12", v.contents, 12);

function g(param) {
  return List.length(/* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

function xx(param) {
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  return List;
}

eq("File \"global_module_alias_test.ml\", line 86, characters 5-12", g(/* () */0), 4);

var V = xx(/* () */0);

eq("File \"global_module_alias_test.ml\", line 92, characters 5-12", Curry._1(V.length, /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]), 3);

eq("File \"global_module_alias_test.ml\", line 93, characters 5-12", v.contents, 15);

var H$1 = f(/* () */0);

eq("File \"global_module_alias_test.ml\", line 95, characters 5-12", Curry._1(H$1.length, /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), 2);

eq("File \"global_module_alias_test.ml\", line 96, characters 5-12", v.contents, 21);

Mt.from_pair_suites("Global_module_alias_test", suites.contents);

var A = /* alias */0;

var B = /* alias */0;

var C = /* alias */0;

var D = /* alias */0;

var E = /* alias */0;

var F = /* alias */0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.A = A;
exports.B = B;
exports.C = C;
exports.D = D;
exports.E = E;
exports.F = F;
exports.v = v;
exports.Make = Make;
exports.f = f;
exports.H = H;
exports.g = g;
exports.xx = xx;
/*  Not a pure module */
