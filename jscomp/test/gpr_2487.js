'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");
var Belt_Array = require("../../lib/js/belt_Array.js");

var b = Belt_Array.eq(/* int array */[
      1,
      2,
      3
    ], /* int array */[
      1,
      2,
      3
    ], Caml_obj.caml_equal);

var A = 0;

exports.A = A;
exports.b = b;
/* b Not a pure module */
