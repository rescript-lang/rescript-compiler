'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

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
  console.log(x);
  console.log(List.length(x));
  return List;
}

var h = f(/* [] */0);

var a = Curry._1(h.length, /* :: */{
      _0: 1,
      _1: /* :: */{
        _0: 2,
        _1: /* :: */{
          _0: 3,
          _1: /* [] */0
        }
      }
    });

eq("File \"module_alias_test.ml\", line 30, characters 6-13", a, 3);

Mt.from_pair_suites("Module_alias_test", suites.contents);

var N;

var V;

var J;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.N = N;
exports.V = V;
exports.J = J;
exports.f = f;
exports.a = a;
/* h Not a pure module */
