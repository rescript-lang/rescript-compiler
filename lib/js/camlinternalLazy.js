'use strict';

var Curry = require("./curry.js");
var Caml_exceptions = require("./caml_exceptions.js");

function from_fun(f) {
  return {
          tag: 246,
          _0: f
        };
}

function from_val(v) {
  var t = v.tag;
  if (t === 250 || t === 246) {
    return {
            tag: 250,
            _0: v
          };
  } else {
    return v;
  }
}

function forward_with_closure(blk, closure) {
  var result = Curry._1(closure, undefined);
  blk._0 = result;
  blk.tag = 250;
  return result;
}

var Undefined = Caml_exceptions.create("CamlinternalLazy.Undefined");

function raise_undefined(param) {
  throw Undefined;
}

function force(lzv) {
  var t = lzv.tag;
  if (t === 250) {
    return lzv._0;
  } else if (t !== 246) {
    return lzv;
  } else {
    var closure = lzv._0;
    lzv._0 = raise_undefined;
    try {
      return forward_with_closure(lzv, closure);
    }
    catch (e){
      lzv._0 = (function (param) {
          throw e;
        });
      throw e;
    }
  }
}

function force_val(lzv) {
  var t = lzv.tag;
  if (t === 250) {
    return lzv._0;
  } else if (t !== 246) {
    return lzv;
  } else {
    var closure = lzv._0;
    lzv._0 = raise_undefined;
    return forward_with_closure(lzv, closure);
  }
}

function is_val(l) {
  return l.tag !== 246;
}

exports.Undefined = Undefined;
exports.force = force;
exports.force_val = force_val;
exports.from_fun = from_fun;
exports.from_val = from_val;
exports.is_val = is_val;
/* No side effect */
