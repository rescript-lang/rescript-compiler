'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Custom_inline = Caml_exceptions.create("Test_literal.Custom_inline");

var v_000 = Custom_inline.ExceptionID;

var v_003 = Custom_inline.Debug;

var v = {
  ExceptionID: v_000,
  _1: 1,
  _2: 2,
  Debug: v_003
};

var vv = [
  1,
  2,
  3
];

var long_v = [
  1,
  2,
  3,
  4,
  5,
  6
];

var long_int_v = [
  1,
  2,
  3,
  4,
  5,
  6
];

var short_int_v = [1];

var empty = [];

exports.Custom_inline = Custom_inline;
exports.v = v;
exports.vv = vv;
exports.long_v = long_v;
exports.long_int_v = long_int_v;
exports.short_int_v = short_int_v;
exports.empty = empty;
/* No side effect */
