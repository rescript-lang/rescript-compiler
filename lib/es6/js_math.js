'use strict';

import * as Pervasives from "./pervasives";

function floor(f) {
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

function ceil(f) {
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

function random_int(min, max) {
  return floor(Math.random() * (max - min | 0)) + min | 0;
}

export {
  floor      ,
  ceil       ,
  random_int ,
  
}
/* No side effect */
