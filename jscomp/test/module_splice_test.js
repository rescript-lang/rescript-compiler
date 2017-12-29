'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var JoinClasses = require("./joinClasses");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
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

var a = JoinClasses(1, 2, 3);

var pair = /* tuple */[
  a,
  6
];

console.log(pair);

eq("File \"module_splice_test.ml\", line 21, characters 5-12", pair);

Mt.from_pair_suites("module_splice_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.a = a;
/* a Not a pure module */
