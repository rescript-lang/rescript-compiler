// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var Obj = require("./obj");
var Caml_exceptions = require("../runtime/caml_exceptions");

var Undefined = [
  248,
  "CamlinternalLazy.Undefined",
  ++ Caml_exceptions.caml_oo_last_id
];

function raise_undefined() {
  throw Undefined;
}

function force_lazy_block(blk) {
  var closure = blk[0];
  blk[0] = raise_undefined;
  try {
    var result = closure(/* () */0);
    blk[0] = result;
    Caml_obj_runtime.caml_obj_set_tag(blk, Obj.forward_tag);
    return result;
  }
  catch (e){
    blk[0] = function () {
      throw e;
    };
    throw e;
  }
}

function force_val_lazy_block(blk) {
  var closure = blk[0];
  blk[0] = raise_undefined;
  var result = closure(/* () */0);
  blk[0] = result;
  Caml_obj_runtime.caml_obj_set_tag(blk, Obj.forward_tag);
  return result;
}

function force(lzv) {
  var t = Caml_obj_runtime.caml_obj_tag(lzv);
  return t === Obj.forward_tag ? lzv[0] : (
            t !== Obj.lazy_tag ? lzv : force_lazy_block(lzv)
          );
}

function force_val(lzv) {
  var t = Caml_obj_runtime.caml_obj_tag(lzv);
  return t === Obj.forward_tag ? lzv[0] : (
            t !== Obj.lazy_tag ? lzv : force_val_lazy_block(lzv)
          );
}

exports.Undefined = Undefined;
exports.force_lazy_block = force_lazy_block;
exports.force_val_lazy_block = force_val_lazy_block;
exports.force = force;
exports.force_val = force_val;
/* No side effect */
