

import * as CamlinternalLazy from "./camlinternalLazy.js";

function from_fun(f) {
  return CamlinternalLazy.from_fun(() => f());
}

let from_val = CamlinternalLazy.from_val;

let Undefined = CamlinternalLazy.Undefined;

let force_val = CamlinternalLazy.force_val;

let is_val = CamlinternalLazy.is_val;

let lazy_from_fun = from_fun;

let lazy_from_val = from_val;

let lazy_is_val = CamlinternalLazy.is_val;

export {
  Undefined,
  force_val,
  from_fun,
  from_val,
  is_val,
  lazy_from_fun,
  lazy_from_val,
  lazy_is_val,
}
/* No side effect */
