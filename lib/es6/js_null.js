

import * as Caml_option from "./caml_option.js";

function test(x) {
  return x === null;
}

function getExn(f) {
  if (f !== null) {
    return f;
  }
  throw new Error("Js.Null.getExn");
}

function bind(x, f) {
  if (x !== null) {
    return f(x);
  } else {
    return null;
  }
}

function iter(x, f) {
  if (x !== null) {
    return f(x);
  }
  
}

function fromOption(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  } else {
    return null;
  }
}

var from_opt = fromOption;

export {
  test ,
  getExn ,
  bind ,
  iter ,
  fromOption ,
  from_opt ,
}
/* No side effect */
