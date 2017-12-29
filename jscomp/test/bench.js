'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function map(f, a) {
  var f$1 = Curry.__1(f);
  var a$1 = a;
  var l = a$1.length;
  if (l) {
    var r = Caml_array.caml_make_vect(l, f$1(a$1[0]));
    for(var i = 1 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
      r[i] = f$1(a$1[i]);
    }
    return r;
  } else {
    return /* array */[];
  }
}

function init(l, f) {
  var l$1 = l;
  var f$1 = Curry.__1(f);
  if (l$1) {
    if (l$1 < 0) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Array.init"
          ];
    } else {
      var res = Caml_array.caml_make_vect(l$1, f$1(0));
      for(var i = 1 ,i_finish = l$1 - 1 | 0; i <= i_finish; ++i){
        res[i] = f$1(i);
      }
      return res;
    }
  } else {
    return /* array */[];
  }
}

function fold_left(f, x, a) {
  var f$1 = Curry.__2(f);
  var x$1 = x;
  var a$1 = a;
  var r = x$1;
  for(var i = 0 ,i_finish = a$1.length - 1 | 0; i <= i_finish; ++i){
    r = f$1(r, a$1[i]);
  }
  return r;
}

function f2() {
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
  return /* () */0;
}

f2(/* () */0);

exports.map = map;
exports.init = init;
exports.fold_left = fold_left;
exports.f2 = f2;
/*  Not a pure module */
