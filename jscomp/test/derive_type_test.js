'use strict';

var Block = require("../../lib/js/block");

var shape = /* array */[
  "x",
  "y"
];

function vv_to_value(x) {
  var args = x;
  return /* Record */Block.__(12, [
            shape,
            /* array */[
              /* Int */Block.__(2, [args[/* x */0]]),
              /* String */Block.__(7, [args[/* y */1]])
            ]
          ]);
}

var x = 3;

exports.x           = x;
exports.vv_to_value = vv_to_value;
/* No side effect */
