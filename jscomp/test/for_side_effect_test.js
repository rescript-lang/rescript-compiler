'use strict';

var Mt = require("./mt.js");

function tst(param) {
  for(var i = (console.log("hi"), 0) ,i_finish = (console.log("hello"), 3); i <= i_finish; ++i){
    
  }
  
}

function test2(param) {
  var v = 0;
  v = 3;
  v = 10;
  for(var i = 0; i <= 1; ++i){
    
  }
  return v;
}

var suites_0 = /* tuple */[
  "for_order",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: 10,
              _1: test2(undefined)
            };
    })
];

var suites = /* :: */{
  _0: suites_0,
  _1: /* [] */0
};

Mt.from_pair_suites("For_side_effect_test", suites);

exports.tst = tst;
exports.test2 = test2;
exports.suites = suites;
/*  Not a pure module */
