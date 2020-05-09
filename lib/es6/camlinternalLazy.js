

import * as Caml_exceptions from "./caml_exceptions.js";

var status_todo = "todo";

var status_done = "done";

function lazy_boxed(l) {
  if (l == null) {
    return false;
  }
  var t = l.RE_LAZY;
  if (t === status_done) {
    return true;
  } else {
    return t === status_todo;
  }
}

function is_val(l) {
  if (l == null) {
    return true;
  } else {
    return l.RE_LAZY !== status_todo;
  }
}

function from_fun(f) {
  return {
          RE_LAZY: status_todo,
          value: f
        };
}

function from_val(v) {
  if (lazy_boxed(v)) {
    return {
            RE_LAZY: status_done,
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

export {
  Undefined ,
  force ,
  force_val ,
  from_fun ,
  from_val ,
  is_val ,
  
}
/* No side effect */
