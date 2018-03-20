'use strict';

var Obj = require("./obj.js");
var Block = require("./block.js");
var Caml_obj = require("./caml_obj.js");
var CamlinternalLazy = require("./camlinternalLazy.js");

function from_fun(f) {
  var x = Block.__(Obj.lazy_tag, [0]);
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

exports.Undefined = Undefined;
exports.force_val = force_val;
exports.from_fun = from_fun;
exports.from_val = from_val;
exports.is_val = is_val;
exports.lazy_from_fun = lazy_from_fun;
exports.lazy_from_val = lazy_from_val;
exports.lazy_is_val = lazy_is_val;
/* No side effect */
