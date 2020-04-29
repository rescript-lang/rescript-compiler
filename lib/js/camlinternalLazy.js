'use strict';

var Caml_exceptions = require("./caml_exceptions.js");

function lazy_boxed(l) {
  if (l == null) {
    return false;
  }
  var t = l.tag;
  if (t === 250) {
    return true;
  } else {
    return t === 246;
  }
}

function is_val(l) {
  if (l == null) {
    return true;
  } else {
    return l.tag !== 246;
  }
}

function from_fun(f) {
  return {
          tag: 246,
          value: f
        };
}

function from_val(v) {
  if (lazy_boxed(v)) {
    return {
            tag: 250,
            value: v
          };
  } else {
    return v;
  }
}

var Undefined = Caml_exceptions.create("CamlinternalLazy.Undefined");

function forward_with_closure(blk, closure) {
  var result = closure();
  blk.value = result;
  blk.tag = 250;
  return result;
}

function raise_undefined() {
  throw {
        ExceptionID: Undefined.ExceptionID,
        Debug: Undefined.Debug
      };
}

function force(lzv) {
  if (lazy_boxed(lzv)) {
    if (is_val(lzv)) {
      return lzv.value;
    } else {
      var closure = lzv.value;
      lzv.value = raise_undefined;
      try {
        return forward_with_closure(lzv, closure);
      }
      catch (e){
        lzv.value = (function () {
            throw e;
          });
        throw e;
      }
    }
  } else {
    return lzv;
  }
}

function force_val(lzv) {
  if (lazy_boxed(lzv)) {
    if (is_val(lzv)) {
      return lzv.value;
    } else {
      var closure = lzv.value;
      lzv.value = raise_undefined;
      return forward_with_closure(lzv, closure);
    }
  } else {
    return lzv;
  }
}

exports.Undefined = Undefined;
exports.force = force;
exports.force_val = force_val;
exports.from_fun = from_fun;
exports.from_val = from_val;
exports.is_val = is_val;
/* No side effect */
