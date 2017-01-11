'use strict';

var Block = require("../../lib/js/block");
var Mt    = require("./mt");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, _z) {
  var y = _z[1];
  var x = _z[0];
  console.log(_z);
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

eq('File "js_string_test.ml", line 13, characters 5-12', /* tuple */[
      "012",
      "0".concat("1", "2")
    ]);

Mt.from_pair_suites("js_string_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
