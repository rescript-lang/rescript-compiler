'use strict';

var $$Array = require("../../lib/js/array.js");
var Bs_Array = require("../../lib/js/bs_Array.js");

function range(i, j) {
  return $$Array.init((j - i | 0) + 1 | 0, (function (k) {
                return k + i | 0;
              }));
}

function randomRange(i, j) {
  var v = Bs_Array.init((j - i | 0) + 1 | 0, (function (k) {
          return k + i | 0;
        }));
  Bs_Array.shuffleInPlace(v);
  return v;
}

exports.range = range;
exports.randomRange = randomRange;
/* No side effect */
