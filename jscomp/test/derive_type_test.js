'use strict';

var Block  = require("../../lib/js/block.js");
var Bs_dyn = require("../../lib/js/bs_dyn.js");

var shape = /* array */[
  "x",
  "y"
];

function vv_to_value(x) {
  var args = x;
  return /* Record */Block.__(12, [
            shape,
            /* array */[
              Bs_dyn.int_to_value(args[/* x */0]),
              Bs_dyn.string_to_value(args[/* y */1])
            ]
          ]);
}

var x = 3;

exports.x           = x;
exports.vv_to_value = vv_to_value;
/* No side effect */
