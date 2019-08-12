'use strict';

var Belt_Array = require("../../lib/js/belt_Array.js");

var b = Belt_Array.eq(/* array */[
      1,
      2,
      3
    ], /* array */[
      1,
      2,
      3
    ], (function (prim, prim$1) {
        return prim === prim$1;
      }));

var A = 0;

exports.A = A;
exports.b = b;
/* b Not a pure module */
