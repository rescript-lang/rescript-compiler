// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Caml_array = require("../runtime/caml_array");
var $$Array    = require("../stdlib/array");
var Caml_curry = require("../runtime/caml_curry");

function test() {
  var v = [
    0,
    0
  ];
  var f = function (_n, _acc) {
    while(true) {
      var acc = _acc;
      var n = _n;
      if (n) {
        _acc = (function(n,acc){
        return function () {
          v[1] += n;
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
  };
  f(10, function () {
        return /* () */0;
      });
  return v[1];
}

function test_closure() {
  var n = 6;
  var v = [
    0,
    0
  ];
  var arr = Caml_array.caml_make_vect(n, function (x) {
        return x;
      });
  for(var i = 0 ,i_finish = n - 1; i<= i_finish; ++i){
    arr[i] = (function(i){
    return function () {
      return i;
    }
    }(i));
  }
  $$Array.iter(function (i) {
        v[1] += Caml_curry.app1(i, 0);
        return /* () */0;
      }, arr);
  return v[1];
}

function test_closure2() {
  var n = 6;
  var v = [
    0,
    0
  ];
  var arr = Caml_array.caml_make_vect(n, function (x) {
        return x;
      });
  for(var i = 0 ,i_finish = n - 1; i<= i_finish; ++i){
    var j = i + i;
    arr[i] = (function(j){
    return function () {
      return j;
    }
    }(j));
  }
  $$Array.iter(function (i) {
        v[1] += Caml_curry.app1(i, 0);
        return /* () */0;
      }, arr);
  return v[1];
}

Mt.from_suites("cps_test.ml", [
      /* :: */0,
      [
        /* tuple */0,
        "cps_test_sum",
        function () {
          return Mt.assert_equal(55, test(/* () */0));
        }
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "cps_test_closure",
          function () {
            return Mt.assert_equal(15, test_closure(/* () */0));
          }
        ],
        [
          /* :: */0,
          [
            /* tuple */0,
            "cps_test_closure2",
            function () {
              return Mt.assert_equal(30, test_closure2(/* () */0));
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
