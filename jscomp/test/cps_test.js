'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");

function test(param) {
  var v = {
    contents: 0
  };
  var f = function (_n, _acc) {
    while(true) {
      var acc = _acc;
      var n = _n;
      if (n === 0) {
        return Curry._1(acc, undefined);
      }
      _acc = (function(n,acc){
      return function (param) {
        v.contents = v.contents + n | 0;
        return Curry._1(acc, undefined);
      }
      }(n,acc));
      _n = n - 1 | 0;
      continue ;
    };
  };
  f(10, (function (param) {
          
        }));
  return v.contents;
}

function test_closure(param) {
  var v = {
    contents: 0
  };
  var arr = Caml_array.caml_make_vect(6, (function (x) {
          return x;
        }));
  for(var i = 0; i <= 5; ++i){
    Caml_array.set(arr, i, (function(i){
        return function (param) {
          return i;
        }
        }(i)));
  }
  $$Array.iter((function (i) {
          v.contents = v.contents + Curry._1(i, 0) | 0;
          
        }), arr);
  return v.contents;
}

function test_closure2(param) {
  var v = {
    contents: 0
  };
  var arr = Caml_array.caml_make_vect(6, (function (x) {
          return x;
        }));
  for(var i = 0; i <= 5; ++i){
    var j = i + i | 0;
    Caml_array.set(arr, i, (function(j){
        return function (param) {
          return j;
        }
        }(j)));
  }
  $$Array.iter((function (i) {
          v.contents = v.contents + Curry._1(i, 0) | 0;
          
        }), arr);
  return v.contents;
}

Mt.from_pair_suites("Cps_test", {
      hd: [
        "cps_test_sum",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: 55,
                    _1: test(undefined)
                  };
          })
      ],
      tl: {
        hd: [
          "cps_test_closure",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: 15,
                      _1: test_closure(undefined)
                    };
            })
        ],
        tl: {
          hd: [
            "cps_test_closure2",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: 30,
                        _1: test_closure2(undefined)
                      };
              })
          ],
          tl: /* [] */0
        }
      }
    });

exports.test = test;
exports.test_closure = test_closure;
exports.test_closure2 = test_closure2;
/*  Not a pure module */
