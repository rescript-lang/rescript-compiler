

import * as Caml_obj from "./caml_obj.js";
import * as Caml_array from "./caml_array.js";
import * as Caml_option from "./caml_option.js";

function caml_weak_create(n) {
  return new Array(n);
}

function caml_weak_set(xs, i, v) {
  if (v !== void 0) {
    xs[i] = Caml_option.valFromOption(v);
    return ;
  }
  
}

function caml_weak_get(xs, i) {
  return Caml_option.undefined_to_opt(xs[i]);
}

function caml_weak_get_copy(xs, i) {
  var match = xs[i];
  if (match !== void 0) {
    return Caml_option.some(Caml_obj.caml_obj_dup(match));
  }
  
}

function caml_weak_check(xs, i) {
  return xs[i] !== void 0;
}

var caml_weak_blit = Caml_array.caml_array_blit;

export {
  caml_weak_create ,
  caml_weak_set ,
  caml_weak_get ,
  caml_weak_get_copy ,
  caml_weak_check ,
  caml_weak_blit ,
  
}
/* No side effect */
