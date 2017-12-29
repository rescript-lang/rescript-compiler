'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Custom_inline = Caml_exceptions.create("Test_literal.Custom_inline");

var v = [
  Custom_inline,
  1,
  2
];

var vv = /* float array */[
  1,
  2,
  3
];

var long_v = /* float array */[
  1,
  2,
  3,
  4,
  5,
  6
];

var long_int_v = /* array */[
  1,
  2,
  3,
  4,
  5,
  6
];

var short_int_v = /* int array */[1];

var empty = /* array */[];

exports.Custom_inline = Custom_inline;
exports.v = v;
exports.vv = vv;
exports.long_v = long_v;
exports.long_int_v = long_int_v;
exports.short_int_v = short_int_v;
exports.empty = empty;
/* No side effect */
