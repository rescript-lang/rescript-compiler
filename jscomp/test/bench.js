'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Pervasives = require("../../lib/js/pervasives.js");

function map(f, a) {
  var f$1 = Curry.__1(f);
  var l = a.length;
  if (l === 0) {
    return [];
  }
  var r = Caml_array.caml_make_vect(l, f$1(a[0]));
  for(var i = 1; i < l; ++i){
    r[i] = f$1(a[i]);
  }
  return r;
}

function init(l, f) {
  var f$1 = Curry.__1(f);
  if (l === 0) {
    return [];
  }
  if (l < 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Array.init",
          Error: new Error()
        };
  }
  var res = Caml_array.caml_make_vect(l, f$1(0));
  for(var i = 1; i < l; ++i){
    res[i] = f$1(i);
  }
  return res;
}

function fold_left(f, x, a) {
  var f$1 = Curry.__2(f);
  var r = x;
  for(var i = 0 ,i_finish = a.length; i < i_finish; ++i){
    r = f$1(r, a[i]);
  }
  return r;
}

function f2(param) {
  var arr = init(3000000, (function (i) {
          return i;
        }));
  var b = map((function (i) {
          return i + i - 1;
        }), arr);
  var v = fold_left((function (prim, prim$1) {
          return prim + prim$1;
        }), 0, b);
  console.log(Pervasives.string_of_float(v));
  
}

f2(undefined);

exports.map = map;
exports.init = init;
exports.fold_left = fold_left;
exports.f2 = f2;
/*  Not a pure module */
