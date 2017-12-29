'use strict';

var Curry = require("./curry.js");
var Caml_oo = require("./caml_oo.js");

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

exports.js = js;
exports.js1 = js1;
exports.js2 = js2;
exports.js3 = js3;
exports.js4 = js4;
exports.js5 = js5;
exports.js6 = js6;
exports.js7 = js7;
exports.js8 = js8;
/* No side effect */
