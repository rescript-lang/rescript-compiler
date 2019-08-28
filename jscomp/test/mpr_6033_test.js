'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

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

function f(x) {
  var y = CamlinternalLazy.force(x);
  return y + "abc";
}

var x = "def";

CamlinternalLazy.force(x);

var u = f(x);

eq("File \"mpr_6033_test.ml\", line 20, characters 6-13", u, "defabc");

Mt.from_pair_suites("Mpr_6033_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.u = u;
/*  Not a pure module */
