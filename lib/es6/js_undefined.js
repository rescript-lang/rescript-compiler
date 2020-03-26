

import * as Caml_option from "./caml_option.js";

function test(x) {
  return x === void 0;
}

function testAny(x) {
  return x === void 0;
}

function getExn(f) {
  if (f !== void 0) {
    return f;
  }
  throw new Error("Js.Undefined.getExn");
}

function bind(x, f) {
  if (x !== void 0) {
    return f(x);
  }
  
}

function iter(x, f) {
  if (x !== void 0) {
    return f(x);
  }
  
}

function fromOption(x) {
  if (x !== void 0) {
    return Caml_option.valFromOption(x);
  }
  
}

var from_opt = fromOption;

export {
  test ,
  testAny ,
  getExn ,
  bind ,
  iter ,
  fromOption ,
  from_opt ,
  
}
/* No side effect */
