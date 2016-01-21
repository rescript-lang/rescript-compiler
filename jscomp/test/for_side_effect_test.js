// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Mt = require("./mt");

function tst() {
  for(var i = (console.log("hi"), 0) ,i_finish = (console.log("hello"), 3); i<= i_finish; ++i){
    
  }
  return /* () */0;
}

function test2() {
  var v = 0;
  v = 3;
  v = 10;
  for(var i = 0; i<= 1; ++i){
    
  }
  return v;
}

var suites_001 = [
  /* tuple */0,
  "for_order",
  function () {
    return Mt.assert_equal(10, test2(/* () */0));
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_suites("for_side_effect_test.ml", suites);

exports.tst    = tst;
exports.test2  = test2;
exports.suites = suites;
/*  Not a pure module */
