'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Int32 = require("../../lib/js/int32.js");
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

eq("File \"limits_test.ml\", line 10, characters 5-12", Pervasives.max_int, (2147483647));

eq("File \"limits_test.ml\", line 11, characters 5-12", Pervasives.min_int, (-2147483648));

eq("File \"limits_test.ml\", line 12, characters 5-12", Int32.max_int, (2147483647));

eq("File \"limits_test.ml\", line 13, characters 5-12", Int32.min_int, (-2147483648));

Mt.from_pair_suites("Limits_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
