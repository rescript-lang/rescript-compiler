'use strict';

var Belt_Array = require("../../lib/js/belt_Array.js");

function range(i, j) {
  return Belt_Array.makeBy((j - i | 0) + 1 | 0, (function (k) {
                return k + i | 0;
              }));
}

function randomRange(i, j) {
  var v = Belt_Array.makeBy((j - i | 0) + 1 | 0, (function (k) {
          return k + i | 0;
        }));
  Belt_Array.shuffleInPlace(v);
  return v;
}

var A = 0;

exports.A = A;
exports.range = range;
exports.randomRange = randomRange;
/* No side effect */
