'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

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
  
}

function f(x) {
  
}

function ff(x) {
  console.log(x);
  
}

eq("File \"ignore_test.ml\", line 16, characters 5-12", void 0, void 0);

Mt.from_pair_suites("Ignore_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.ff = ff;
/*  Not a pure module */
