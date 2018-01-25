'use strict';

var Bs_Array = require("../../lib/js/bs_Array.js");

function range(i, j) {
  return Bs_Array.makeBy((j - i | 0) + 1 | 0, (function (k) {
                return k + i | 0;
              }));
}

function randomRange(i, j) {
  var v = Bs_Array.makeBy((j - i | 0) + 1 | 0, (function (k) {
          return k + i | 0;
        }));
  Bs_Array.shuffle(v);
  return v;
}

var A = 0;

exports.A = A;
exports.range = range;
exports.randomRange = randomRange;
/* No side effect */
