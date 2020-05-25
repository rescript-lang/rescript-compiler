'use strict';

var Mt = require("./mt.js");
var JoinClasses = require("./joinClasses");
var Caml_splice_call = require("../../lib/js/caml_splice_call.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
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

function joinClasses(prim) {
  return Caml_splice_call.spliceApply(JoinClasses, [prim]);
}

var a = JoinClasses(1, 2, 3);

var pair = /* tuple */[
  a,
  6
];

console.log(pair);

eq("File \"module_splice_test.ml\", line 21, characters 5-12", pair);

Mt.from_pair_suites("Module_splice_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.joinClasses = joinClasses;
exports.a = a;
/* a Not a pure module */
