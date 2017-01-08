'use strict';

var Mt         = require("./mt");
var Block      = require("../../lib/js/block");
var Curry      = require("../../lib/js/curry");
var Caml_array = require("../../lib/js/caml_array");
var $$Array    = require("../../lib/js/array");

function test() {
  var v = [0];
  var f = function (n, acc) {
    var _n = n;
    var _acc = Curry.__1(acc);
    while(true) {
      var acc$1 = _acc;
      var n$1 = _n;
      if (n$1) {
        _acc = (function(n$1,acc$1){
        return function () {
          v[0] = v[0] + n$1 | 0;
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
  };
  f(10, function () {
        return /* () */0;
      });
  return v[0];
}

function test_closure() {
  var v = [0];
  var arr = Caml_array.caml_make_vect(6, function (x) {
        return x;
      });
  for(var i = 0; i <= 5; ++i){
    arr[i] = (function(i){
    return function () {
      return i;
    }
    }(i));
  }
  $$Array.iter(function (i) {
        v[0] = v[0] + Curry._1(i, 0) | 0;
        return /* () */0;
      }, arr);
  return v[0];
}

function test_closure2() {
  var v = [0];
  var arr = Caml_array.caml_make_vect(6, function (x) {
        return x;
      });
  for(var i = 0; i <= 5; ++i){
    var j = i + i | 0;
    arr[i] = (function(j){
    return function () {
      return j;
    }
    }(j));
  }
  $$Array.iter(function (i) {
        v[0] = v[0] + Curry._1(i, 0) | 0;
        return /* () */0;
      }, arr);
  return v[0];
}

Mt.from_pair_suites("cps_test.ml", /* :: */[
      /* tuple */[
        "cps_test_sum",
        function () {
          return /* Eq */Block.__(0, [
                    55,
                    test(/* () */0)
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          "cps_test_closure",
          function () {
            return /* Eq */Block.__(0, [
                      15,
                      test_closure(/* () */0)
                    ]);
          }
        ],
        /* :: */[
          /* tuple */[
            "cps_test_closure2",
            function () {
              return /* Eq */Block.__(0, [
                        30,
                        test_closure2(/* () */0)
                      ]);
            }
          ],
          /* [] */0
        ]
      ]
    ]);

exports.test          = test;
exports.test_closure  = test_closure;
exports.test_closure2 = test_closure2;
/*  Not a pure module */
