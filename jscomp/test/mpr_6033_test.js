'use strict';

var Mt = require("./mt.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

function f(x) {
  var y = CamlinternalLazy.force(x);
  return y + "abc";
}

var x = {
  RE_LAZY_DONE: true,
  value: "def"
};

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
