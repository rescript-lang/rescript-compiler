'use strict';

let CamlinternalLazy = require("./camlinternalLazy.js");

function from_fun(f) {
  return CamlinternalLazy.from_fun(() => {
    return f();
  });
}

let from_val = CamlinternalLazy.from_val;

let Undefined = CamlinternalLazy.Undefined;

let force_val = CamlinternalLazy.force_val;

let is_val = CamlinternalLazy.is_val;

let lazy_from_fun = from_fun;

let lazy_from_val = from_val;

let lazy_is_val = CamlinternalLazy.is_val;

exports.Undefined = Undefined;
exports.force_val = force_val;
exports.from_fun = from_fun;
exports.from_val = from_val;
exports.is_val = is_val;
exports.lazy_from_fun = lazy_from_fun;
exports.lazy_from_val = lazy_from_val;
exports.lazy_is_val = lazy_is_val;
/* No side effect */
