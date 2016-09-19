'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

var keys = ( function (x){return Object.keys(x)});


  function $$high_order(x){
   return function(y,z){
      return x + y + z 
   }
  }

;

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

eq('File "ffi_js.ml", line 24, characters 5-12', /* tuple */[
      6,
      $$high_order(1)(2, 3)
    ]);

Mt.from_pair_suites("ffi_js.ml", suites[0]);

exports.keys    = keys;
exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/* keys Not a pure module */
