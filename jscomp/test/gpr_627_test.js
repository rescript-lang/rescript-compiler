'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

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

var u = {
  say: (function (x, y) {
      return x + y | 0;
    })
};

var v = {
  hi: (function (x, y) {
      var self = this ;
      var u = {
        x: x
      };
      return self.say(u.x) + y + x;
    }),
  say: (function (x) {
      var self = this ;
      return x * self.x();
    }),
  x: (function () {
      return 3;
    })
};

var p_001 = u.say(1, 2);

var p = /* tuple */[
  3,
  p_001
];

eq("File \"gpr_627_test.ml\", line 26, characters 5-12", p);

eq("File \"gpr_627_test.ml\", line 27, characters 5-12", /* tuple */[
      v.hi(1, 2),
      6
    ]);

Mt.from_pair_suites("Gpr_627_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
exports.v = v;
/* u Not a pure module */
