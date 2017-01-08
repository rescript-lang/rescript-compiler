'use strict';

var Curry      = require("../../lib/js/curry");
var Caml_array = require("../../lib/js/caml_array");

function f(n, acc) {
  var _n = n;
  var _acc = Curry.__1(acc);
  while(true) {
    var acc$1 = _acc;
    var n$1 = _n;
    if (n$1) {
      _acc = (function(n$1,acc$1){
      return function () {
        console.log("" + n$1);
        return acc$1(/* () */0);
      }
      }(n$1,acc$1));
      _n = n$1 - 1 | 0;
      continue ;
      
    }
    else {
      return acc$1(/* () */0);
    }
  };
}

function test_closure() {
  var arr = Caml_array.caml_make_vect(6, function (x) {
        return x;
      });
  for(var i = 0; i <= 6; ++i){
    arr[i] = (function(i){
    return function () {
      return i;
    }
    }(i));
  }
  return arr;
}

f(10, function () {
      return /* () */0;
    });

exports.f            = f;
exports.test_closure = test_closure;
/*  Not a pure module */
