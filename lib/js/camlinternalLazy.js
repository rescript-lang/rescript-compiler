'use strict';

var Block = require("./block.js");
var Curry = require("./curry.js");
var Caml_obj = require("./caml_obj.js");
var Caml_exceptions = require("./caml_exceptions.js");

function new_block_with_tag(tag, value) {
  var x = /* obj_block */Block.__(tag, [0]);
  x[0] = value;
  return x;
}

function from_fun(f) {
  return new_block_with_tag(246, f);
}

function from_val(v) {
  var t = v.tag | 0;
  if (t === 250 || t === 246 || false) {
    return new_block_with_tag(250, v);
  } else {
    return v;
  }
}

function forward_with_closure(blk, closure) {
  var result = Curry._1(closure, undefined);
  blk[0] = result;
  Caml_obj.caml_obj_set_tag(blk, 250);
  return result;
}

var Undefined = Caml_exceptions.create("CamlinternalLazy.Undefined");

function raise_undefined(param) {
  throw Undefined;
}

function force(lzv) {
  var t = lzv.tag | 0;
  if (t === 250) {
    return lzv[0];
  } else if (t !== 246) {
    return lzv;
  } else {
    var closure = lzv[0];
    lzv[0] = raise_undefined;
    try {
      return forward_with_closure(lzv, closure);
    }
    catch (e){
      lzv[0] = (function (param) {
          throw e;
        });
      throw e;
    }
  }
}

function force_val(lzv) {
  var t = lzv.tag | 0;
  if (t === 250) {
    return lzv[0];
  } else if (t !== 246) {
    return lzv;
  } else {
    var closure = lzv[0];
    lzv[0] = raise_undefined;
    return forward_with_closure(lzv, closure);
  }
}

function is_val(l) {
  return (l.tag | 0) !== 246;
}

exports.Undefined = Undefined;
exports.force = force;
exports.force_val = force_val;
exports.from_fun = from_fun;
exports.from_val = from_val;
exports.is_val = is_val;
/* No side effect */
