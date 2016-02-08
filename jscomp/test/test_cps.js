// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_array = require("../runtime/caml_array");
var Caml_curry = require("../runtime/caml_curry");

function f(_n, _acc) {
  while(true) {
    var acc = _acc;
    var n = _n;
    if (n) {
      _acc = (function(n,acc){
      return function () {
        console.log("" + n);
        return Caml_curry.app1(acc, /* () */0);
      }
      }(n,acc));
      _n = n - 1;
      continue ;
      
    }
    else {
      return Caml_curry.app1(acc, /* () */0);
    }
  };
}

function test_closure() {
  var n = 6;
  var arr = Caml_array.caml_make_vect(n, function (x) {
        return x;
      });
  for(var i = 0; i<= n; ++i){
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
