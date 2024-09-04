

import * as Caml_exceptions from "./caml_exceptions.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";

function is_val(l) {
  return l.LAZY_DONE;
}

let Undefined = /* @__PURE__ */Caml_exceptions.create("CamlinternalLazy.Undefined");

function forward_with_closure(blk, closure) {
  let result = closure();
  blk.VAL = result;
  blk.LAZY_DONE = true;
  return result;
}

function raise_undefined() {
  throw Caml_js_exceptions.internalMakeExn(Undefined);
}

function force(lzv) {
  if (lzv.LAZY_DONE) {
    return lzv.VAL;
  } else {
    let closure = lzv.VAL;
    lzv.VAL = raise_undefined;
    try {
      return forward_with_closure(lzv, closure);
    } catch (e) {
      lzv.VAL = () => {
        throw e;
      };
      throw e;
    }
  }
}

function force_val(lzv) {
  if (lzv.LAZY_DONE) {
    return lzv.VAL;
  } else {
    let closure = lzv.VAL;
    lzv.VAL = raise_undefined;
    return forward_with_closure(lzv, closure);
  }
}

function from_fun(closure) {
  return {
    LAZY_DONE: false,
    VAL: closure
  };
}

function from_val(value) {
  return {
    LAZY_DONE: true,
    VAL: value
  };
}

export {
  Undefined,
  force,
  force_val,
  is_val,
  from_fun,
  from_val,
}
/* No side effect */
