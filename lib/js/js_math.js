'use strict';

var Pervasives = require("./pervasives.js");

function ceil_int(f) {
  if (f > Pervasives.max_int) {
    return Pervasives.max_int;
  } else if (f < Pervasives.min_int) {
    return Pervasives.min_int;
  } else {
    return Math.ceil(f);
  }
}

function floor_int(f) {
  if (f > Pervasives.max_int) {
    return Pervasives.max_int;
  } else if (f < Pervasives.min_int) {
    return Pervasives.min_int;
  } else {
    return Math.floor(f);
  }
}

function random_int(min, max) {
  return floor_int(Math.random() * (max - min | 0)) + min | 0;
}

exports.ceil_int   = ceil_int;
exports.floor_int  = floor_int;
exports.random_int = random_int;
/* No side effect */
