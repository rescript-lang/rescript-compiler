

import * as Obj from "./obj.js";
import * as Curry from "./curry.js";
import * as Caml_obj from "./caml_obj.js";
import * as Caml_exceptions from "./caml_exceptions.js";

var Undefined = Caml_exceptions.create("CamlinternalLazy.Undefined");

function raise_undefined(param) {
  throw Undefined;
}

function force_lazy_block(blk) {
  var closure = blk[0];
  blk[0] = raise_undefined;
  try {
    var result = Curry._1(closure, void 0);
    blk[0] = result;
    Caml_obj.caml_obj_set_tag(blk, Obj.forward_tag);
    return result;
  }
  catch (e){
    blk[0] = (function (param) {
        throw e;
      });
    throw e;
  }
}

function force_val_lazy_block(blk) {
  var closure = blk[0];
  blk[0] = raise_undefined;
  var result = Curry._1(closure, void 0);
  blk[0] = result;
  Caml_obj.caml_obj_set_tag(blk, Obj.forward_tag);
  return result;
}

function force(lzv) {
  var t = lzv.tag | 0;
  if (t === Obj.forward_tag) {
    return lzv[0];
  } else if (t !== Obj.lazy_tag) {
    return lzv;
  } else {
    return force_lazy_block(lzv);
  }
}

function force_val(lzv) {
  var t = lzv.tag | 0;
  if (t === Obj.forward_tag) {
    return lzv[0];
  } else if (t !== Obj.lazy_tag) {
    return lzv;
  } else {
    return force_val_lazy_block(lzv);
  }
}

export {
  Undefined ,
  force_lazy_block ,
  force_val_lazy_block ,
  force ,
  force_val ,
  
}
/* No side effect */
