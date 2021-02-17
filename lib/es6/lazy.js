

import * as Curry from "./curry.js";
import * as CamlinternalLazy from "./camlinternalLazy.js";

function from_fun(f) {
  return {
          LAZY_DONE: false,
          VAL: (function () {
              return Curry._1(f, undefined);
            })
        };
}

function from_val(v) {
  return {
          LAZY_DONE: true,
          VAL: v
        };
}

var Undefined = CamlinternalLazy.Undefined;

var force_val = CamlinternalLazy.force_val;

var is_val = CamlinternalLazy.is_val;

var lazy_from_fun = from_fun;

var lazy_from_val = from_val;

var lazy_is_val = CamlinternalLazy.is_val;

export {
  Undefined ,
  force_val ,
  from_fun ,
  from_val ,
  is_val ,
  lazy_from_fun ,
  lazy_from_val ,
  lazy_is_val ,
  
}
/* No side effect */
