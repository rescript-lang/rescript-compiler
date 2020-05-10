

import * as Caml_exceptions from "./caml_exceptions.js";

var status_done = "done";

function is_val(l) {
  return l.RE_LAZY === status_done;
}

var Undefined = Caml_exceptions.create("CamlinternalLazy.Undefined");

function forward_with_closure(blk, closure) {
  var result = closure();
  blk.value = result;
  blk.RE_LAZY = status_done;
  return result;
}

function raise_undefined() {
  throw {
        RE_EXN_ID: Undefined,
        Error: new Error()
      };
}

function force(lzv) {
  if (lzv.RE_LAZY === status_done) {
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
}

function force_val(lzv) {
  if (lzv.RE_LAZY === status_done) {
    return lzv.value;
  } else {
    var closure = lzv.value;
    lzv.value = raise_undefined;
    return forward_with_closure(lzv, closure);
  }
}

export {
  Undefined ,
  force ,
  force_val ,
  is_val ,
  
}
/* No side effect */
