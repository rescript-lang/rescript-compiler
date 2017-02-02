'use strict';

var $$Array    = require("../../lib/js/array");
var Pervasives = require("../../lib/js/pervasives");

function print_arr(foo) {
  return $$Array.iter(Pervasives.print_int, foo);
}

exports.print_arr = print_arr;
/* No side effect */
