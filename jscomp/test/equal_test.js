'use strict';


function str_equal(x, y) {
  return +(x === y);
}

var str_b = /* true */1;

function int_equal(x, y) {
  return +(x === y);
}

var v = /* false */0;

exports.str_equal = str_equal;
exports.str_b = str_b;
exports.int_equal = int_equal;
exports.v = v;
/* str_b Not a pure module */
