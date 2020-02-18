

import * as Obj from "./obj.js";
import * as Block from "./block.js";
import * as Caml_obj from "./caml_obj.js";
import * as CamlinternalLazy from "./camlinternalLazy.js";

function from_fun(f) {
  var x = /* obj_block */Block.__(Obj.lazy_tag, [0]);
  x[0] = f;
  return x;
}

function from_val(v) {
  var t = v.tag | 0;
  if (t === Obj.forward_tag || t === Obj.lazy_tag || t === Obj.double_tag) {
    return Caml_obj.caml_lazy_make_forward(v);
  } else {
    return v;
  }
}

function is_val(l) {
  return (l.tag | 0) !== Obj.lazy_tag;
}

var Undefined = CamlinternalLazy.Undefined;

var force_val = CamlinternalLazy.force_val;

var lazy_from_fun = from_fun;

var lazy_from_val = from_val;

var lazy_is_val = is_val;

export {
  Undefined ,
  force_val ,
  from_fun ,
  from_val ,
  is_val ,
  lazy_from_fun ,
  lazy_from_val ,
  lazy_is_val ,
  
}
/* No side effect */
