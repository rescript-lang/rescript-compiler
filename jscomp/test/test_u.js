'use strict';

var Pervasives = require("../../lib/js/pervasives.js");

function f(x) {
  var v = /* record */{
    contents: x
  };
  var sum = 0;
  while(v.contents > 0) {
    sum = sum + v.contents | 0;
    Pervasives.decr(v);
  };
  return sum;
}

exports.f = f;
/* No side effect */
