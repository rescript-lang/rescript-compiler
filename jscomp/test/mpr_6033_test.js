'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
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

function f(x) {
  var tag = x.tag | 0;
  var y = tag === 250 ? x[0] : (
      tag === 246 ? CamlinternalLazy.force_lazy_block(x) : x
    );
  return y + "abc";
}

var x = "def";

var tag = x.tag | 0;

if (tag !== 250) {
  if (tag === 246) {
    CamlinternalLazy.force_lazy_block(x);
  }
  
}

var u = f(x);

eq("File \"mpr_6033_test.ml\", line 20, characters 6-13", u, "defabc");

Mt.from_pair_suites("mpr_6033_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.u = u;
/*  Not a pure module */
