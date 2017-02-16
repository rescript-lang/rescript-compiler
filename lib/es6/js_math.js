'use strict';

import * as Pervasives from "./pervasives";

function unsafe_ceil(prim) {
  return Math.ceil(prim);
}

function ceil_int(f) {
  if (f > Pervasives.max_int) {
    return Pervasives.max_int;
  }
  else if (f < Pervasives.min_int) {
    return Pervasives.min_int;
  }
  else {
    return Math.ceil(f);
  }
}

function unsafe_floor(prim) {
  return Math.floor(prim);
}

function floor_int(f) {
  if (f > Pervasives.max_int) {
    return Pervasives.max_int;
  }
  else if (f < Pervasives.min_int) {
    return Pervasives.min_int;
  }
  else {
    return Math.floor(f);
  }
}

function random_int(min, max) {
  return floor_int(Math.random() * (max - min | 0)) + min | 0;
}

var ceil = ceil_int;

var floor = floor_int;

export {
  unsafe_ceil  ,
  ceil_int     ,
  ceil         ,
  unsafe_floor ,
  floor_int    ,
  floor        ,
  random_int   ,
  
}
/* No side effect */
