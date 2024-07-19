

import * as Curry from "./curry.js";
import * as Caml_option from "./caml_option.js";

function bind(x, f) {
  if (x == null) {
    return x;
  } else {
    return Curry._1(f, x);
  }
}

function iter(x, f) {
  if (!(x == null)) {
    return Curry._1(f, x);
  }
  
}

function fromOption(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  }
  
}

let from_opt = fromOption;

export {
  bind,
  iter,
  fromOption,
  from_opt,
}
/* No side effect */
