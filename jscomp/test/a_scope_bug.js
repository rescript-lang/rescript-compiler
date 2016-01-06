// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("../stdlib/pervasives");

function odd(z) {
  var even$1 = z * z;
  var a = even$1 + 4 + even$1;
  console.log(Pervasives.string_of_int(a));
  return even(32);
}

function even(y) {
  return odd(y);
}

exports.odd = odd;
exports.even = even;
/* No side effect */
