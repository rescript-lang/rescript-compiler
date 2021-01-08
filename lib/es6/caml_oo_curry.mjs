

import * as Curry from "./curry.mjs";
import * as Caml_oo from "./caml_oo.mjs";

function js(label, cacheid, obj, args) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return Curry.app(meth, args);
}

function js1(label, cacheid, a0) {
  return Curry._1(Caml_oo.caml_get_public_method(a0, label, cacheid), a0);
}

function js2(label, cacheid, a0, a1) {
  return Curry._2(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1);
}

function js3(label, cacheid, a0, a1, a2) {
  return Curry._3(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2);
}

function js4(label, cacheid, a0, a1, a2, a3) {
  return Curry._4(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3);
}

function js5(label, cacheid, a0, a1, a2, a3, a4) {
  return Curry._5(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3, a4);
}

function js6(label, cacheid, a0, a1, a2, a3, a4, a5) {
  return Curry._6(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3, a4, a5);
}

function js7(label, cacheid, a0, a1, a2, a3, a4, a5, a6) {
  return Curry._7(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3, a4, a5, a6);
}

function js8(label, cacheid, a0, a1, a2, a3, a4, a5, a6, a7) {
  return Curry._8(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3, a4, a5, a6, a7);
}

export {
  js ,
  js1 ,
  js2 ,
  js3 ,
  js4 ,
  js5 ,
  js6 ,
  js7 ,
  js8 ,
  
}
/* No side effect */
