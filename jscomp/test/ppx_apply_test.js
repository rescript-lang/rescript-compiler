'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: /* tuple */[
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

var u = 3;

function nullary() {
  return 3;
}

function unary(a) {
  return a + 3 | 0;
}

var xx = unary(3);

eq("File \"ppx_apply_test.ml\", line 17, characters 5-12", u, 3);

function h(a) {
  return xx(a);
}

Mt.from_pair_suites("Ppx_apply_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
exports.nullary = nullary;
exports.unary = unary;
exports.xx = xx;
exports.h = h;
/* u Not a pure module */
