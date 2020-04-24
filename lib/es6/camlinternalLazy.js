

import * as Block from "./block.js";
import * as Curry from "./curry.js";
import * as Caml_obj from "./caml_obj.js";
import * as Caml_exceptions from "./caml_exceptions.js";

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

export {
  Undefined ,
  force ,
  force_val ,
  from_fun ,
  from_val ,
  is_val ,
  
}
/* No side effect */
