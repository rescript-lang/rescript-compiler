

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

let Undefined = CamlinternalLazy.Undefined;

let force_val = CamlinternalLazy.force_val;

let is_val = CamlinternalLazy.is_val;

let lazy_from_fun = from_fun;

let lazy_from_val = from_val;

let lazy_is_val = CamlinternalLazy.is_val;

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
