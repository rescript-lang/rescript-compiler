'use strict';

let Caml_exceptions = require("./caml_exceptions.js");

function is_val(l) {
  return l.LAZY_DONE;
}

let Undefined = /* @__PURE__ */Caml_exceptions.create("Primitive_lazy.Undefined");

function forward_with_closure(blk, closure) {
  let result = closure();
  blk.VAL = result;
  blk.LAZY_DONE = true;
  return result;
}

function raise_undefined() {
  throw {
    RE_EXN_ID: Undefined,
    Error: new Error()
  };
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

exports.Undefined = Undefined;
exports.force = force;
exports.force_val = force_val;
exports.from_fun = from_fun;
exports.from_val = from_val;
exports.is_val = is_val;
/* No side effect */