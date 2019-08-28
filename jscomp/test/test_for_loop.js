'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");

function for_(x) {
  for(var i = 0 ,i_finish = (console.log("hi"), x.length); i <= i_finish; ++i){
    console.log(Caml_array.caml_array_get(x, i));
  }
  return /* () */0;
}

function for_2(x) {
  for(var i = 0 ,i_finish = x.length; i <= i_finish; ++i){
    console.log(Caml_array.caml_array_get(x, i));
  }
  return /* () */0;
}

function for_3(x) {
  var v = /* record */{
    contents: 0
  };
  var arr = $$Array.map((function (param, param$1) {
          return /* () */0;
        }), x);
  for(var i = 0 ,i_finish = x.length; i <= i_finish; ++i){
    var j = (i << 1);
    Caml_array.caml_array_set(arr, i, (function(j){
        return function (param) {
          v.contents = v.contents + j | 0;
          return /* () */0;
        }
        }(j)));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, /* () */0);
        }), arr);
  return v.contents;
}

function for_4(x) {
  var v = /* record */{
    contents: 0
  };
  var arr = $$Array.map((function (param, param$1) {
          return /* () */0;
        }), x);
  for(var i = 0 ,i_finish = x.length; i <= i_finish; ++i){
    var j = (i << 1);
    var k = (j << 1);
    Caml_array.caml_array_set(arr, i, (function(k){
        return function (param) {
          v.contents = v.contents + k | 0;
          return /* () */0;
        }
        }(k)));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, /* () */0);
        }), arr);
  return v.contents;
}

function for_5(x, u) {
  var v = /* record */{
    contents: 0
  };
  var arr = $$Array.map((function (param, param$1) {
          return /* () */0;
        }), x);
  for(var i = 0 ,i_finish = x.length; i <= i_finish; ++i){
    var k = Caml_int32.imul((u << 1), u);
    Caml_array.caml_array_set(arr, i, (function(k){
        return function (param) {
          v.contents = v.contents + k | 0;
          return /* () */0;
        }
        }(k)));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, /* () */0);
        }), arr);
  return v.contents;
}

function for_6(x, u) {
  var v = /* record */{
    contents: 0
  };
  var arr = $$Array.map((function (param, param$1) {
          return /* () */0;
        }), x);
  var v4 = /* record */{
    contents: 0
  };
  var v5 = /* record */{
    contents: 0
  };
  Pervasives.incr(v4);
  for(var j = 0; j <= 1; ++j){
    Pervasives.incr(v5);
    var v2 = /* record */{
      contents: 0
    };
    (function(v2){
    for(var i = 0 ,i_finish = x.length; i <= i_finish; ++i){
      var k = Caml_int32.imul((u << 1), u);
      var h = (v5.contents << 1);
      Pervasives.incr(v2);
      Caml_array.caml_array_set(arr, i, (function(k,h){
          return function (param) {
            v.contents = (((((v.contents + k | 0) + v2.contents | 0) + u | 0) + v4.contents | 0) + v5.contents | 0) + h | 0;
            return /* () */0;
          }
          }(k,h)));
    }
    }(v2));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, /* () */0);
        }), arr);
  return v.contents;
}

exports.for_ = for_;
exports.for_2 = for_2;
exports.for_3 = for_3;
exports.for_4 = for_4;
exports.for_5 = for_5;
exports.for_6 = for_6;
/* No side effect */
