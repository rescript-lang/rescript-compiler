// GENERATED CODE BY BUCKLESCRIPT VERSION 0.4.1 , PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../caml_builtin_exceptions");
var Curry                   = require("../curry");
var Caml_array              = require("../caml_array");
var $$Array                 = require("../array");

var v = [0];

function f() {
  var arr = Caml_array.caml_make_vect(10, function () {
        return /* () */0;
      });
  for(var i = 0; i <= 9; ++i){
    arr[i] = (function(i){
    return function () {
      v[0] = v[0] + i | 0;
      return /* () */0;
    }
    }(i));
  }
  return arr;
}

var u = f(/* () */0);

$$Array.iter(function (x) {
      return Curry._1(x, /* () */0);
    }, u);

if (v[0] !== 45) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "test_closure.ml",
          53,
          2
        ]
      ];
}

exports.v = v;
exports.f = f;
/* u Not a pure module */
