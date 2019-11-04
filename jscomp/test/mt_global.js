'use strict';

var Block = require("../../lib/js/block.js");

function collect_eq(test_id, suites, loc, x, y) {
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

function collect_neq(test_id, suites, loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Neq */Block.__(1, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

function collect_approx(test_id, suites, loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Approx */Block.__(5, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

exports.collect_eq = collect_eq;
exports.collect_neq = collect_neq;
exports.collect_approx = collect_approx;
/* No side effect */
