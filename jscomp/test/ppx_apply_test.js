'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  Pervasives.incr(test_id);
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

var u = 3;

function nullary() {
  return 3;
}

function unary(a) {
  return a + 3 | 0;
}

var xx = unary(3);

eq("File \"ppx_apply_test.ml\", line 17, characters 5-12", u, 3);

Mt.from_pair_suites("Ppx_apply_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
exports.nullary = nullary;
exports.unary = unary;
exports.xx = xx;
/* u Not a pure module */
