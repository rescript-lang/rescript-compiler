'use strict';

var Caml_exceptions = require("./caml_exceptions.js");

function is_val(l) {
  return l.LAZY_DONE;
}

var Undefined = /* @__PURE__ */Caml_exceptions.create("CamlinternalLazy.Undefined");

function forward_with_closure(blk, closure) {
  var result = closure();
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
    var closure = lzv.VAL;
    lzv.VAL = raise_undefined;
    try {
      return forward_with_closure(lzv, closure);
    }
    catch (e){
      lzv.VAL = (function () {
          throw e;
        });
      throw e;
    }
  }
}

function force_val(lzv) {
  if (lzv.LAZY_DONE) {
    return lzv.VAL;
  } else {
    var closure = lzv.VAL;
    lzv.VAL = raise_undefined;
    return forward_with_closure(lzv, closure);
  }
}

exports.Undefined = Undefined;
exports.force = force;
exports.force_val = force_val;
exports.is_val = is_val;
/* No side effect */
