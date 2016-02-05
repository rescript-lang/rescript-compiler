// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var CamlinternalLazy = require("./camlinternalLazy");

function from_fun(f) {
  var x = Caml_obj_runtime.caml_obj_block(246, 1);
  x[0] = f;
  return x;
}

function from_val(v) {
  var t = Caml_obj_runtime.caml_obj_tag(v);
  if (t === 250 || t === 246 || t === 253) {
    return Caml_obj_runtime.caml_lazy_make_forward(v);
  }
  else {
    return v;
  }
}

function is_val(l) {
  return +(Caml_obj_runtime.caml_obj_tag(l) !== 246);
}

var Undefined = CamlinternalLazy.Undefined;

var force_val = CamlinternalLazy.force_val;

var lazy_from_fun = from_fun;

var lazy_from_val = from_val;

var lazy_is_val = is_val;

exports.Undefined     = Undefined;
exports.force_val     = force_val;
exports.from_fun      = from_fun;
exports.from_val      = from_val;
exports.is_val        = is_val;
exports.lazy_from_fun = lazy_from_fun;
exports.lazy_from_val = lazy_from_val;
exports.lazy_is_val   = lazy_is_val;
/* No side effect */
