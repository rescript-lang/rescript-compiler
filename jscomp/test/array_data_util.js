'use strict';

var Bs_Array = require("../../lib/js/bs_Array.js");

function range(i, j) {
  return Bs_Array.init((j - i | 0) + 1 | 0, (function (k) {
                return k + i | 0;
              }));
}

function randomRange(i, j) {
  return Bs_Array.shuffle(Bs_Array.init((j - i | 0) + 1 | 0, (function (k) {
                    return k + i | 0;
                  })));
}

var A = 0;

exports.A = A;
exports.range = range;
exports.randomRange = randomRange;
/* No side effect */
