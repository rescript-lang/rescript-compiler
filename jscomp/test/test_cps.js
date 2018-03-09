'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");

function f(_n, _acc) {
  while(true) {
    var acc = _acc;
    var n = _n;
    if (n === 0) {
      return Curry._1(acc, /* () */0);
    } else {
      _acc = (function(n,acc){
      return function () {
        console.log(String(n));
        return Curry._1(acc, /* () */0);
      }
      }(n,acc));
      _n = n - 1 | 0;
      continue ;
      
    }
  };
}

function test_closure() {
  var arr = Caml_array.caml_make_vect(6, (function (x) {
          return x;
        }));
  for(var i = 0; i <= 6; ++i){
    Caml_array.caml_array_set(arr, i, (function(i){
        return function () {
          return i;
        }
        }(i)));
  }
  return arr;
}

f(10, (function () {
        return /* () */0;
      }));

exports.f = f;
exports.test_closure = test_closure;
/*  Not a pure module */
