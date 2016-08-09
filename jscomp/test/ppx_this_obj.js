'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

var v = {
  x: function () {
    return 3;
  },
  say: function (x) {
    var self = this ;
    return x * self.x();
  },
  hi: function (x, y) {
    var self = this ;
    return self.say(x) + y;
  }
};

eq('File "ppx_this_obj.ml", line 25, characters 5-12', /* tuple */[
      11,
      v.hi(3, 2)
    ]);

Mt.from_pair_suites("ppx_this_obj.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.v       = v;
/* v Not a pure module */
