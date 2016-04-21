// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Curry      = require("../runtime/curry");
var Caml_array = require("../runtime/caml_array");
var $$Array    = require("../stdlib/array");

function test() {
  var v = [0];
  var f = function (_n, _acc) {
    while(true) {
      var acc = _acc;
      var n = _n;
      if (n) {
        _acc = (function(n,acc){
        return function () {
          v[0] = v[0] + n | 0;
          return Curry._1(acc, /* () */0);
        }
        }(n,acc));
        _n = n - 1 | 0;
        continue ;
        
      }
      else {
        return Curry._1(acc, /* () */0);
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
  for(var i = 0; i<= 5; ++i){
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
  for(var i = 0; i<= 5; ++i){
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
          return /* Eq */{
                  0: 55,
                  1: test(/* () */0),
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "cps_test_closure",
          function () {
            return /* Eq */{
                    0: 15,
                    1: test_closure(/* () */0),
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            "cps_test_closure2",
            function () {
              return /* Eq */{
                      0: 30,
                      1: test_closure2(/* () */0),
                      length: 2,
                      tag: 0
                    };
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
