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

var v = /* record */{
  contents: 3
};

function update(param) {
  Pervasives.incr(v);
  return true;
}

Pervasives.incr(v);

eq("File \"gpr_1762_test.ml\", line 22, characters 6-13", v.contents, 4);

Mt.from_pair_suites("Gpr_1762_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.update = update;
/*  Not a pure module */
