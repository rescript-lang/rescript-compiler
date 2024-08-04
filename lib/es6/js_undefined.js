

import * as Js_exn from "./js_exn.js";
import * as Caml_option from "./caml_option.js";

function test(x) {
  return x === undefined;
}

function testAny(x) {
  return x === undefined;
}

function getExn(f) {
  if (f !== undefined) {
    return f;
  }
  throw new Error(new Error("Js.Undefined.getExn").RE_EXN_ID, {
    cause: new Error("Js.Undefined.getExn")
  });
}

function bind(x, f) {
  if (x !== undefined) {
    return f(x);
  }
  
}

function iter(x, f) {
  if (x !== undefined) {
    return f(x);
  }
  
}

function fromOption(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  }
  
}

let from_opt = fromOption;

export {
  test,
  testAny,
  getExn,
  bind,
  iter,
  fromOption,
  from_opt,
}
/* Js_exn Not a pure module */
