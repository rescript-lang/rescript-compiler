// GENERATED CODE BY BUCKLESCRIPT VERSION 0.3 , PLEASE EDIT WITH CARE
'use strict';

var Bench = require("benchmark");
var Curry = require("../runtime/curry");

var suite = new Bench.Suite();

suite.add("first", function () {
      return /* () */0;
    }, {
      "onComplete": function () {
        return /* () */0;
      }
    });

Curry._3(suite.add.bind(suite), "second", function () {
      return /* () */0;
    }, {
      "onComplete": function () {
        return /* () */0;
      }
    });

Curry._2(suite.on.bind(suite), "complete", function () {
      for(var i = 0 ,i_finish = suite.length - 1 | 0; i <= i_finish; ++i){
        console.log(suite[i]);
      }
      return /* () */0;
    });

Curry._1(suite.run.bind(suite), /* () */0);

exports.suite = suite;
/* suite Not a pure module */
