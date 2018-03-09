'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
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

var v = {
  x: (function () {
      return 3;
    }),
  say: (function (x) {
      var self = this ;
      return x * self.x();
    }),
  hi: (function (x, y) {
      var self = this ;
      return self.say(x) + y;
    })
};

var v2 = {
  hi: (function (x, y) {
      var self = this ;
      return self.say(x) + y;
    }),
  say: (function (x) {
      var self = this ;
      return x * self.x();
    }),
  x: (function () {
      return 3;
    })
};

var v3 = {
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

var v4 = {
  hi: (function (x, y) {
      return x + y;
    }),
  say: (function (x) {
      return x;
    }),
  x: (function () {
      return 1;
    })
};

var collection = /* array */[
  v,
  v2,
  v3,
  v4
];

eq("File \"ppx_this_obj_test.ml\", line 59, characters 5-12", /* tuple */[
      11,
      v.hi(3, 2)
    ]);

eq("File \"ppx_this_obj_test.ml\", line 60, characters 5-12", /* tuple */[
      11,
      v2.hi(3, 2)
    ]);

Mt.from_pair_suites("ppx_this_obj_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.v2 = v2;
exports.v3 = v3;
exports.v4 = v4;
exports.collection = collection;
/* v Not a pure module */
